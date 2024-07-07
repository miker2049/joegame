(in-package cl-user)
(defpackage :server.html
  (:use :cl :spinneret))
(in-package :server.html)


(let ((*html-style* :tree))
  (with-html
    (:div
     (:p ("hey hey [some](~a) hey" "some.org")))))



