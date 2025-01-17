(defpackage joegame-assets (:use :cl)
            (:export asset-path get-asset-path))

(in-package joegame-assets)

(defun asset-path ()
  (format nil "~A"
          (asdf:system-source-directory "joegame-assets")))

(defun get-asset-path (p)
  (concatenate 'string (asset-path) p))

