(in-package :cl-user)
(defpackage server.config
  (:use :cl)
  (:import-from
   envy
   config-env-var
   defconfig)
  (:export config
           *application-root*
           *static-directory*
           *template-directory*
           appenv
           developmentp
           productionp))
(in-package :server.config)

(setf (config-env-var) "APP_ENV")

(defparameter *world-root*   (asdf:system-source-directory :world))
(defparameter *application-root*   (asdf:system-source-directory :server))
(defparameter *static-directory*   (merge-pathnames #P"static/" *application-root*))
(defparameter *template-directory* (merge-pathnames #P"templates/" *application-root*))

(if nil 2 3)

(defconfig :common
    `(:databases ((:maindb :sqlite3 :database-name ,(if (uiop:getenv-pathname "SERVER_DB")
                                                        (uiop:getenv-pathname "SERVER_DB")
                                                        (merge-pathnames #P"db.db" *world-root*))))
      :worldmap-size 40
      :worldmap-tile-size 250))

(defconfig |development|
    '())

(defconfig |production|
    '())

(defconfig |test|
    '())

(defun config (&optional key)
  (envy:config #.(package-name *package*) key))

(defun appenv ()
  (uiop:getenv (config-env-var #.(package-name *package*))))

(defun developmentp ()
  (string= (appenv) "development"))

(defun productionp ()
  (string= (appenv) "production"))
