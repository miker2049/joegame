(defsystem "golf-2d"
  :version "0.0.1"
  :author "bb"
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
  :description "Simple golf"
  :defsystem-depends-on (#:deploy)
  :build-operation "deploy-op"
  :build-pathname #P"golf-2d"
  :entry-point "golf-2d:main")
