(asdf:defsystem "server"
  :version "0.1.0"
  :author ""
  :license ""
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "server-bin"
  :entry-point "server:start"
  :depends-on (
               "clack"
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
               "spinneret"
               "spinneret/cl-markdown"
               ;; for DB
               "datafly"
               "sxql"
               ;; added
               "world"
               "zip")
  :components ((:module "templates"
                :components ((:static-file "index.djhtml")))
               (:module "src"
                :components
                ((:file "main" :depends-on ("view" "db" "asset-db"))
                 (:file "api" :depends-on ("web"))
                 (:file "web" :depends-on ("view"))
                 (:file "view" :depends-on ("config"))
                 (:file "db" :depends-on ("config"))
                 (:file "config")
                 (:file "asset-db" :depends-on ("db")))))
  :description ""
  :in-order-to ((test-op (test-op "server/test"))))

(defsystem "server/test"
  :defsystem-depends-on ("prove-asdf")
  :author ""
  :license ""
  :depends-on ("server"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "server-tests"))))
  :description "Test system for tile-server"
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))

