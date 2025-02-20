(in-package :cl-user)
(defpackage server.view
  (:use :cl)
  (:import-from server.config
                *template-directory*)
  (:import-from caveman2
                *response*
                response-headers)
  (:import-from djula
                add-template-directory
                compile-template*
                render-template*)

  (:import-from datafly
                encode-json)
  (:export render
           render-json))
(in-package :server.view)

(djula:add-template-directory *template-directory*)

(defparameter *template-registry* (make-hash-table :test 'equal))

(defun render (template-path &optional env)
  (let ((template (gethash template-path *template-registry*)))
    (unless template
      (setf template (djula:compile-template* (princ-to-string template-path)))
      (setf (gethash template-path *template-registry*) template))
    (apply #'djula:render-template*
           template nil
           env)))

(defun render-json (object)
  (setf (getf (response-headers *response*) :content-type) "application/json")
  (encode-json object))


;;
;; Execute package definition

(defpackage server.djula
  (:use :cl)
  (:import-from :config
   :config
                :appenv
   :developmentp
                :productionp)
  (:import-from :caveman2
   :url-for))

;; (setf djula:*djula-execute-package* (find-package :server.djula))
