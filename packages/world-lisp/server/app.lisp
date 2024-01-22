(ql:quickload :world-server)

(defpackage server.app
  (:use :cl)
  (:import-from lack.builder
                builder)
  (:import-from ppcre
                scan
                regex-replace)
  (:import-from server.web
                *web*)
  (:import-from config
                config
                productionp
                *static-directory*))
(in-package :server.app)

(builder
 (:static
  :path (lambda (path)
          (if (ppcre:scan "^(?:/images/|/tiles/|/css/|/js/|/robot\\.txt$|/favicon\\.ico$)" path)
              path
              nil))
  :root *static-directory*)
 (if (productionp)
     nil
     :accesslog)
 (if (getf (config) :error-log)
     `(:backtrace
       :output ,(getf (config) :error-log))
     nil)
 :session
 (if (productionp)
     nil
     (lambda (app)
       (lambda (env)
         (let ((datafly:*trace-sql* t))
           (funcall app env)))))
 *web*)
