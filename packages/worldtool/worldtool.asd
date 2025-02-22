(defsystem "worldtool"
  :version "0.0.1"
  :author "mik"
  :license "MIT"
  :depends-on (#:alexandria
               #:dissect
               #:cl-liballegro
               #:cl-liballegro-nuklear
               #:glkit
               #:cl-opengl
               #:livesupport)
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "main")
                 (:file "game"))))
  :description "Simple editor for world"
  :defsystem-depends-on (#:deploy)
  :build-operation "deploy-op"
  :build-pathname #P"worldtool"
  :entry-point "worldtool:main")
