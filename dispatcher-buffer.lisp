(in-package #:tesseract)

(declaim (optimize (debug 3) (safety 3) (speed 0)))

(defvar *dispatcher-event-base*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;    the ZMQ routing happens here.                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; create the dispatch buffer closures

(defun make-dispatcher-io-buffer (socket who port disconnector
				  &key (max-bytes 16384))
  (let (;(in-message-queue (make-queue))
	(out-message-queue (make-queue))
	(bytes-left-to-write 0)
	(read-handler-registered nil)
	(write-handler-registered nil)
	(eof-seen nil))
    
    ;; closures follow

    (labels
	;; If this function notices that there is data to write, it
	;; will set the io-handler on the socket for the write
	;; handler. If the function notices it has read >= than the
	;; max-bytes it will remove itself from the handler *after*
	;; ensuring the write handler is set up properly.
	((read-a-line (fd event exception)
	  (declare (ignorable fd event exception)) 
	   ;; establish the zmq context here, because the read-a-line
	   ;; closure is the entry and exit point for the zmq
	   ;; messaging, and every thread needs its own context.
	   (zmq:with-context (context) ;; the number defines context threads
	     (handler-case
		 (let* ((line (format nil "~A~%" (read-line socket))) ; add the newline
			(reply (backend-question context line))) 
		   (log-message :debug "~&[read-a-line] Read from router-dealer::  ~A" reply)
		   
		   (incf bytes-left-to-write (length reply))
		   (enqueue reply out-message-queue)
		   (queue-count out-message-queue)
		   ;; (break)
		   (when (> bytes-left-to-write 0)
		     ;; if the write handler isn't registered, register it
		     ;; now since we have data to write.
		     (log-message :debug "||bytes-left-to-write>>> ~A" bytes-left-to-write)
		     (unless write-handler-registered
		       (set-io-handler *dispatcher-event-base*
				       (socket-os-fd socket)
				       :write
				       #'write-a-line)
		       (setf write-handler-registered t)))
		   ;; If there is more data than we should be reading,
		   ;; remove ourselves from the io handler. when the write
		   ;; handler notices that, after writing some data, more of
		   ;; it can be read, it will reregister the io handler for
		   ;; the read socket.
		   (when (>= bytes-left-to-write max-bytes)
		     (funcall disconnector who port :read)
		     (setf read-handler-registered nil)))

	       (socket-connection-reset-error ()
		 ;; if the client resets, we shut everything down.
		 (log-message :debug "~&Client ~A:~A: Connection reset by peer." who port)
		 (funcall disconnector who port :close))
	       (end-of-file ()
		 ;; actually, when we get here, we should be dispatching
		 ;; to our service handlers via zeromq, but as this is a
		 ;; simple test of the architecutre, we're just going to
		 ;; complain via format and unregister the read end of
		 ;; the socket... usually.
		 (log-message :debug "~&Client ~A:~A produced end-of-file on a read. This is normal."
			 who port)
		 (if (zerop bytes-left-to-write)
		     (funcall disconnector who port :close)
		     (progn
		     (funcall disconnector who port :read)
		     (setf read-handler-registered nil)
		     (setf eof-seen t)))))))

	 ;; This function will notice that if it has written enough
	 ;; bytes to bring the bytes-left-to-write under max-bytes, it
	 ;; will re-register the reader io handler. If there is no
	 ;; data to write, it will, after ensuring the read handler is
	 ;; registered, unregister itself as to not be called
	 ;; constantly on a write ready socket with no data to write.
	 
	 (write-a-line (fd event exception)
	   (handler-case
	       (progn
		 ;; if we have something to write to the client, do so.
		 (when (> bytes-left-to-write 0)
		   (let ((line (dequeue out-message-queue)))
		     (format socket "~A~%" line)
		     (finish-output socket)
		     (log-message :debug "~&Wrote to ~A:~A: ~A" who port line)
		     (decf bytes-left-to-write (length line))))

		 ;; if we have fallen below the max-bytes mark,
		 ;; re-register the read handler to get more
		 ;; data. Don't reregister the read handler if we've
		 ;; seen that the client closed our read end of the
		 ;; socket.

		 (when (< bytes-left-to-write max-bytes)
		   (unless (or eof-seen read-handler-registered)
		     (set-io-handler *dispatcher-event-base*
				     (socket-os-fd socket)
				     :read
				     #'read-a-line)
		     (setf read-handler-registered t)))

		 ;; if we don't have any data to write _and_ we have
		 ;; seen the end of file fromt he client, then we
		 ;; close the connection to the client since it will
		 ;; never speak to us again and we're done speaking to
		 ;; it.
		 ;;
		 ;; If we have written all of our data and there might
		 ;; be more to do later, then unregister the write
		 ;; handler so we don't get called unecessarily. This
		 ;; might mean that sometimes we will make an extra
		 ;; trip through the event-dispatcher to perform the
		 ;; write if we read more from the client and it
		 ;; reregisters us.

		 (when (zerop bytes-left-to-write)
		   (if eof-seen
		       (funcall disconnector who port :close)
		       (progn
			 (funcall disconnector who port :write)
			 (setf write-handler-registered nil)))))
	     (socket-connection-reset-error ()
	       ;; make sure the connection is closed if we get a
	       ;; reset. Usually this won't happen, unless you have
	       ;; been fucking around with the flow in this example.
	       (log-message :debug "Client ~A:~A connection reset by peer." who port)
	       (funcall disconnector who port :close))
	     (hangup ()
	       ;; in this server, if the client doesn't accept data,
	       ;; it also means it will never send us data again. So
	       ;; close the connection for good.
	       (log-message :debug "Client ~A:~A got hangup on write." who port)
	       (funcall disconnector who port :close)))))

      ;; this is the closure that is returned from
      ;; make-dispatcher-io-buffer which allows us access to the
      ;; read/writer in the scope of the closure. We will ask for the
      ;; correct functions when setting up the io handlers. NOTE: by
      ;; simply asking for the handler, we have assumed it is to be
      ;; immediately put into an iolib event handler. This is why they
      ;; are considered registered at this point.

      (lambda (msg)
	;; (declare (ignorable context))
	(cond
	  ((equalp msg :read-a-line)
	   (setf read-handler-registered t)
	   #'read-a-line)
	  ((equalp msg :write-a-line)
	   (setf write-handler-registered t)
	   #'write-a-line)
	  (t
	   (error "make-dispatcher-buffer: Please supply :read-a-line or :write-a-line~%")))))))
