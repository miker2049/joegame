(in-package :cl-user)
(defpackage server-test
  (:use cl
        server
        prove))
(in-package :server-test)

(plan 3)

(is 4 4)
(is 5 5)
(is 1 5)
;; blah blah blah.

(finalize)
