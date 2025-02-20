(in-package :cl-user)
(defpackage server.db
  (:use :cl)
  (:import-from server.config
                config)
  (:import-from datafly
                *connection*)
  (:import-from cl-dbi
                connect-cached)
  (:export connection-settings
           db
           table-count
           with-connection))
(in-package :server.db)

(defun connection-settings (&optional (db :maindb))
  (cdr (assoc db (config :databases))))

(defun db (&optional (db :maindb))
  (apply #'connect-cached (connection-settings db)))

(defmacro with-connection (conn &body body)
  `(let ((*connection* ,conn))
     ,@body))

