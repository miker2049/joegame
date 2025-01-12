(defsystem "dark-work-you-are-a-wizard"
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
  :description "Dark game where you need to navigate."
  :defsystem-depends-on (#:deploy)
  :build-operation "deploy-op"
  :build-pathname #P"dark-work-you-are-a-wizard"
  :entry-point "dark-work-you-are-a-wizard:main")
