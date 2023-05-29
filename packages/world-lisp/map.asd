
(in-package #:cl-user)

(asdf:defsystem #:map
    :name "map"
  :description "Lisp code for mapscripts"
  :components ((:file "render")
                  (:file "simplex")
                  (:file "worldconf")
                  (:file "gen" :depends-on ("render")))
  :depends-on (#:png #:cffi #:alexandria #:jonathan)
  :defsystem-depends-on ("cffi-grovel"))
