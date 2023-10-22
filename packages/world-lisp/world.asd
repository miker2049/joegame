(defsystem "world"
  :name "world"
  :description "Lisp code for mapscripts"
  :components (
               (:file "package" :depends-on ("worldconf"))
               (:file "grid")
               (:file "db" :depends-on ("render" "utils"))
               (:file "render" :depends-on ("grid" "utils"))
               (:file "simplex")
               (:file "draw-image" :depends-on ("render"))
               (:file "utils" )
               (:file "magick" )
               (:file "async")
               (:file "data")
               (:file "tiled" :depends-on ("config" "utils" "simplex"))
               (:file "config")

               (:file "worldconf-utils" :depends-on
                      ("magick" "tiled" "async" "utils" "simplex" "grid" "render" "db"))
               (:file "worldconf" :depends-on ("worldconf-utils"))
               (:file "worldconf-debug" :depends-on ("worldconf")))
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
               #:cl-async)
  :in-order-to ((test-op (test-op "world/tests"))))

(defsystem "world/tests"
  :depends-on (#:fiveam "world")
  :components ((:file "test"))
  :perform (test-op (o c)
                    (symbol-call :fiveam
                                 '#:run! (find-symbol* :world-test-suite :world-tests))))
