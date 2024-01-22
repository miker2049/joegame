(in-package :cl-user)
(defpackage server.js
  (:use cl
        caveman2
        config
        server.view
        server.db
        parenscript)
  (:export keyboard-listener))
(in-package :server.js)


(defparameter get-scroll-div (ps (defparameter scroll-div ((@ document get-element-by-id) "container"))))

(ps
  (var scroll-div ((@ document get-element-by-id) "container")))
(defparameter keyboard-listener
  (ps ((@ window add-event-listener) "keydown"
       #'(lambda (ev)
           (case (@ ev key)
             ("ArrowLeft" ((@ scroll-div scroll-by) (create left -10)))
             ("ArrowRight" ((@ scroll-div scroll-by) (create left 10)))
             ("ArrowUp" ((@ scroll-div scroll-by) (create top -10)))
             ("ArrowDown" ((@ scroll-div scroll-by) (create top 10))))))))
