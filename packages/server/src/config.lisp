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

(defparameter *application-root*   (asdf:system-source-directory :world/server))
(defparameter *static-directory*   (merge-pathnames #P"server/static/" *application-root*))
(defparameter *template-directory* (merge-pathnames #P"server/templates/" *application-root*))

(defconfig :common
    `(:databases ((:maindb :sqlite3 :database-name "/home/mik/joegame/packages/world-lisp/db.db"))
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
