(in-package :cl-user)
(defpackage server
  (:use :cl)
  (:import-from config
                config)
  (:import-from clack
                clackup)
  (:export start
           start*
           stop))
(in-package :server)

(defvar *appfile-path*
  (asdf:system-relative-pathname :world/server #P"server/app.lisp"))

(defvar *handler* nil)

(defun start (&rest args &key server port debug &allow-other-keys)
  (declare (ignore server port debug))
  (when *handler*
    (restart-case (error "Server is already running.")
      (restart-server ()
        :report "Restart the server"
        (stop))))
  (setf *handler*
        (apply #'clackup *appfile-path* (append args '(:use-thread t)))))

(defun stop ()
  (prog1
      (clack:stop *handler*)
    (setf *handler* nil)))

(defun start* ()
  (handler-case
      (progn
        (start)
        (loop))
    (sb-sys:interactive-interrupt ()
      (progn
        (format *error-output* "Goodbye!")
        (stop)
        (sb-ext:exit)))))
