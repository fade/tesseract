;; -*-lisp-*-
(defpackage :tesseract
            (:use :cl)
            (:use :tesseract.app-utils)
            (:export :-main))

(in-package :tesseract)


;; This holds any open connections to clients as keys in the
;; table. The values are a list containing the host and port of the
;; connection. We use this to close all connections to the clients, if
;; any, when the server exits.  This allows all clients to notice the
;; server had gone away.


(defvar *open-connections*)

;; the multiplexer selects on a listening socket which performs a
;; blocking accept and then creates a closure which acts as a buffer
;; between reading the wire and writing the server response
;; back to the client. 

(defun run-server-helper (port)
  "The multiplexer selects on a listening socket which performs a
  blocking accept and then creates a closure which acts as a buffer
  between reading the wire and writing the server response back to the
  client."
  (with-open-socket
      (server :connect :passive
              :address-family :internet
              :type :stream
              :ipv6 nil
              :external-format '(:utf-8 :eol-style :crlf))
    (log-message :debug "Created socket: ~A[fd=~A]" server (socket-os-fd server))

    ;; Bind the socket to all interfaces with the specifiec port
    (bind-address server +ipv4-unspecified+ :port port :reuse-addr t)
    (log-message :debug "Bound socket: ~A" server)

    ;; Start listening on the server socket.
    (listen-on server :backlog 10)
    (log-message :debug "Listening on socket bound to: ~A:~A"
	         (local-host server)
	         (local-port server))

    ;; Set up the listener handler for any incoming clients
    (set-io-handler *event-base*
                    (socket-os-fd server)
                    :read
                    (make-listener-handler server))

    ;; Accept connections forever
    (handler-case
        (event-dispatch *dispatch-event-base*)
      (socket-connection-reset-error ()
        (log-message :debug "~&Caught unexpected connection reset by peer."))
      (hangup ()
        (log-message :debug "~&Caught unexpected hangup! Client closed connection on write!"))
      (end-of-file ()
        (log-message :debug "~&Caught unexpected end-of-file! Client closed connection on read!")))))

(defun listen-for-it (socket)
  "return an anonymous function closing over socket such that it can
  listen for connections."
  (lambda (fd event exception)
    (declare (ignorable fd event exception))
    ;; do a blocking accept, return nil of no socket is available.
    (let* ((client (accept-connection socket :wait t)))
      (when client
        (multiple-value-bind (who port)
            (remote-name client)
          (log-message :debug "Accepted a client from ~A:~A" who port)
          ;; save the client connection so we can close it later.
          (sb-ext:with-locked-hash-table (*open-connections*)
            (setf (gethash `(,who ,port) *open-connections*) client))
          (let ((io-buffer
                  (make-io-buffer client who port
                                             (make-server-disconnector client))))
            ;; set up the line echo function for the client socket.
            ;; The internals of the buffer will perform the
            ;; appropriate registration/deregistration protocol on the
            ;; handlers at the right time, depending upon data
            ;; availability.

            (set-io-handler *event-base*
                            (socket-os-fd client)
                            :read
                            (funcall io-buffer :read-a-line))
            (set-io-handler *event-base*
                            (socket-os-fd client)
                            :write
                            (funcall io-buffer :write-a-line)))
          )))
    ))

;; global socket destructifier.
(defun make-server-disconnector (socket)
  "Global socket destructifier."
  (lambda (who port &rest events)
    (let ((fd (socket-os-fd socket)))
      (if (not (intersection '(:read :write :error) events))
	  (remove-fd-handlers *event-base* fd :read t :write t :error t)
	  (progn
	    (when (member :read events)
	      (remove-fd-handlers *event-base* fd :read t))
	    (when (member :write events)
	      (remove-fd-handlers *event-base* fd :write t))
	    (when (member :error events)
	      (remove-fd-handlers *event-base* fd :error t)))))
    ;; finally close the socket if we're 
    (when (member :close events)
      (log-message :debug "Closing connection to ~A:~A" who port)
      (finish-output)
      (close socket)
      (sb-ext:with-locked-hash-table (*open-connections*)
	(remhash `(,who ,port) *open-connections*)))))


(defun -main (&optional args)
  (format t "My Call sig::~%~{~A~^ ~}~%~%" args))

