(in-package :worldtool)

(defclass container ()
  ((x :accessor x-pos :initarg x :initform 0)
   (y :accessor y-pos :initarg y :initform 0)
   (scale :accessor scale :initarg scale :initform 1)
   (children :accessor children :initarg children :initform '())
   (rotation :accessor rotation :initarg rotation :initform 0)))
