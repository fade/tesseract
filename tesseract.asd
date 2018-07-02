;; -*-lisp-*-
;;;; tesseract.asd

(asdf:defsystem #:tesseract
  :description "The protocol/server component of the user agent software stack."
  :author "Brian O'Reilly <fade@deepsky.com>"
  :license "Modified BSD License"
  ;; :serial t
  :depends-on (:IOLIB
               :CL-LOG
               :CL-PPCRE
               :ALEXANDRIA
               :SB-CONCURRENCY
               :OPTIMA
               :RUTILS
               :DRAKMA
               :HUNCHENTOOT
               :LPARALLEL
               :CHANL
               :ZEROMQ)
  :pathname "./"
  :components ((:file "packages")
               (:file "app-utils" :depends-on ("packages"))
               (:file "dispatcher-buffer" :depends-on ("packages"))
               (:file "tesseract" :depends-on ("dispatcher-buffer" "packages"))))

