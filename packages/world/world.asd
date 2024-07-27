(defsystem "world"
  :name "world"
  :description "Lisp code for mapscripts"
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "world"
  :entry-point "world:main"
  :components (
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
               (:file "tiled" :depends-on ("config" "utils" "simplex"))
               (:file "config")
               (:file "setup" :depends-on ("config" "worldconf" "tiled"))
               (:file "worldconf-utils" :depends-on
                      ("magick" "config" "tiled" "async" "utils" "simplex" "grid" "draw-image" "db"))
               (:file "worldconf" :depends-on ("worldconf-utils"))
               (:file "worldconf-debug" :depends-on ("worldconf"))
               (:file "worldtiles" :depends-on ("worldconf"))
               (:file "joegame" :depends-on ("worldconf")))
  :depends-on ("alexandria"
               "sqlite"
               "cl-tiled"
               "png"
               "colored"
               "cffi-libffi"
               "envy"
               "clingon"
               "assets"
               "ironclad"
               "jonathan"
               "bordeaux-threads"
               "blackbird"
               "cl-async")
  :in-order-to ((test-op (test-op "world/tests"))))

(defsystem "world/tests"
  :depends-on (#:fiveam "world")
  :components ((:file "test"))
  :perform (test-op (o c)
                    (symbol-call :fiveam
                                 '#:run! (find-symbol* :world-test-suite :world-tests))))
