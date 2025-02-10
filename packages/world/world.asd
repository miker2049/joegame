(defsystem "world"
  :name "world"
  :description "Lisp code for mapscripts"
  :components ((:module "sqlite-world" :components ((:file "sqlite-world" )) :depends-on ("utils"))
               (:file "package" :depends-on ("worldconf"))
               (:file "grid")
               (:file "db" :depends-on ("render" "utils"))
               (:file "render" :depends-on ("grid" "utils"))
               (:file "simplex" :depends-on ("render"))
               (:file "draw-image" :depends-on ("render"))
               (:file "utils" )
               (:file "magick" :depends-on ("utils") )
               (:file "async")
               (:file "data")
               (:file "server" :depends-on ("worldtiles" "worldconf" "csp"))
               (:file "tiled" :depends-on ("config" "utils" "simplex"))
               (:file "config")
               (:file "worldconf-utils" :depends-on
                      ("magick" "config" "tiled" "async" "utils" "simplex" "grid" "draw-image" "db"))
               (:file "worldconf" :depends-on ("worldconf-utils"))
               (:file "worldconf-debug" :depends-on ("worldconf"))
               (:file "small-world" :depends-on ("worldconf"))
               (:file "csp" :depends-on ("worldconf" "utils" "grid" "data"))
               (:file "worldtiles" :depends-on ("worldconf" "small-world" "sqlite-world" "csp")))
  :depends-on ("alexandria"
               "sqlite"
               "cl-tiled"
               "png"
               "colored"
               "cffi-libffi"
               "ningle"
               "clack"
               "envy"
               "clingon"
               "joegame-assets"
               "ironclad"
               "jonathan"
               "bordeaux-threads"
               "lparallel"
               "blackbird"
               "cl-async"
               "nodgui")
  :in-order-to ((test-op (test-op "world/tests"))))

(defsystem "world/tests"
  :depends-on (#:fiveam "world")
  :components ((:file "test"))
  :perform (test-op (o c)
                    (symbol-call :fiveam
                                 '#:run! (find-symbol* :world-test-suite :world-tests))))
