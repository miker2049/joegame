(defpackage assets (:use :cl)
            (:export asset-path get-asset-path))

(in-package assets)

(defun asset-path ()
  (format nil "~A"
          (asdf:system-source-directory "assets")))

(defun get-asset-path (p)
  (concatenate 'string (asset-path) p))

