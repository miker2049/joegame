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
                      ("magick" "tiled" "async" "utils" "simplex" "grid" "draw-image" "db"))
               (:file "worldconf" :depends-on ("worldconf-utils"))
               (:file "worldconf-debug" :depends-on ("worldconf"))
               (:file "joegame" :depends-on ("worldconf")))
  :depends-on ("alexandria"
               "sqlite"
               "cl-tiled"
               "png"
               "colored"
               "cffi-libffi"
               "mito"
               "clingon"
               "jonathan"
               "bordeaux-threads"
               "blackbird"
               "cl-async")
  :in-order-to ((test-op (test-op "world/tests"))))

(asdf:defsystem "world-server"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("clack"
               "lack"
               "envy"
               "cl-ppcre"
               "uiop"
               "caveman2"
               "png"
               "cl-who"
               ;; for @route annotation
               "cl-syntax-annot"
               "parenscript"

               ;; HTML Template
               "djula"
               "world"

               ;; for DB
               "datafly"
               "sxql")
  :components ((:module "server/src"
                :components
                ((:file "main" :depends-on ("config" "view" "db" "asset-db"))
                 (:file "api" :depends-on ("web"))
                 (:file "web" :depends-on ("view"))
                 (:file "view" :depends-on ("config"))
                 (:file "db" :depends-on ("config"))
                 (:file "asset-db" :depends-on ("db"))
                 (:file "config"))))
  :description ""
  :in-order-to ((test-op (test-op "server-test"))))

(defsystem "server-test"
  :defsystem-depends-on ("prove-asdf")
  :author ""
  :license ""
  :depends-on ("world-server"
               "prove")
  :components ((:module "server/tests"
                :components
                ((:test-file "server-tests"))))
  :description "Test system for tile-server"
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))

(defsystem "world/tests"
  :depends-on (#:fiveam "world")
  :components ((:file "test"))
  :perform (test-op (o c)
                    (symbol-call :fiveam
                                 '#:run! (find-symbol* :world-test-suite :world-tests))))
