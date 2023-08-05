
(in-package #:cl-user)

(asdf:defsystem "world"
    :name "world"
    :description "Lisp code for mapscripts"
    :components (
                    (:file "package" :depends-on ("worldconf"))
                    (:file "grid")
                    (:file "db" :depends-on ("render" "utils"))
                    (:file "render" :depends-on ("grid"))
                    (:file "simplex")
                    (:file "draw-image" :depends-on ("render"))
                    (:file "utils" )
                    (:file "async")
                    (:file "data")
                    (:file "tiled" :depends-on ("config"))
                    (:file "config")
                    (:file "worldconf-utils" :depends-on ("async" "utils" "simplex" "grid" "render" "db"))
                    (:file "worldconf" :depends-on ("worldconf-utils"))
                    (:file "gen" :depends-on ("render")))
    :depends-on (#:alexandria
                    #:sqlite
                    #:cl-tiled
                    #:png
                    #:colored
                    #:cffi-libffi
                    #:mito
                    #:clingon
                    #:jonathan
                    #:bordeaux-threads
                    #:blackbird
                    #:cl-async))
