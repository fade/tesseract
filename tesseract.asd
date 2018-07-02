;; -*-lisp-*-
;;;; tesseract.asd

(asdf:defsystem #:tesseract
  :description "The protocol/server component of the user agent software stack."
  :author "Brian O'Reilly <fade@deepsky.com>"
  :license "Modified BSD License"
  :serial t
  :depends-on (:IOLIB
               :CL-LOG
               :CL-PPCRE
               ;; #+sbcl
               ;; :SB-CONCURRENCY
               :ALEXANDRIA
               :OPTIMA
               :RUTILS
               :DRAKMA
               :HUNCHENTOOT
               :LPARALLEL
               :CHANL
               :ZEROMQ)
  :pathname "./"
  :components ((:file "app-utils")
               (:file "tesseract")))

