(defsystem "worldtool"
  :version "0.0.1"
  :author "mik"
  :license "MIT"
  :depends-on (#:alexandria
               
               #:cl-liballegro
               #:cl-liballegro-nuklear
               
               #:livesupport)
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "main"))))
  :description "Simple editor for world"
  :defsystem-depends-on (#:deploy)
  :build-operation "deploy-op"
  :build-pathname #P"worldtool"
  :entry-point "worldtool:main")
