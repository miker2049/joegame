(in-package :cl-user)
(defpackage config
  (:use :cl)
  (:import-from
   envy
   config-env-var
   defconfig)
  (:export config
           *application-root*
           *static-directory*
           *template-directory*
           *terrain-directory*
           *tiles-directory*
           *tiled-version*
           *tiled-json-version*
           appenv
           developmentp
           productionp))
(in-package :config)


(defun dotenv (&optional (filepath (merge-pathnames (truename ".") ".env")))
  "Reads the environment file, splits it up, and injects into current environment."
  (with-open-file (s filepath :if-does-not-exist nil)
    (loop for line = (read-line s nil)
          while line do
            (let ((parts (utils:split line :delim #\=)))
              (alexandria:if-let ((envkey (first parts))
                                  (envval (first (reverse parts))))
                (setf (uiop:getenv envkey) envval))))))

(defvar *tiled-json-version* "1.10")
(defvar *tiled-version* "1.10.1")

(setf (config-env-var) "APP_ENV")

(defparameter *application-root*   (asdf:system-source-directory :world))
(defparameter *static-directory*   (merge-pathnames #P"server/static/" *application-root*))
(defparameter *terrain-directory* (merge-pathnames #P"terrains/" *static-directory*))
(defparameter *tiles-directory* (merge-pathnames #P"tiles/" *static-directory*))
(defparameter *template-directory* (merge-pathnames #P"server/templates/" *application-root*))

(defconfig :common
    `(:databases ((:maindb :sqlite3 :database-name ,(merge-pathnames #P"db.db" *application-root*)))
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
