(defpackage :tesseract.app-utils
  (:use :cl)
  (:export :internal-disable-debugger)
  (:export :internal-quit))

(defpackage :tesseract
            (:use :cl)
            (:use :tesseract.app-utils)
            (:use #:cl-log
                  #:sb-concurrency
                  #:iolib
                  #:iolib.sockets)
            (:export :-main))

