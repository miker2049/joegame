(defpackage utils (:use :cl)
  (:export
    enumerate
    e-distance
    miles-to-tiles
    tiles-to-miles))
(in-package utils)

(defun e-distance (x1 y1 x2 y2)
    (sqrt
        (+
            (expt (- x2 x1) 2)
            (expt (- y2 y1) 2))))

(defun miles-to-tiles (miles)
    (* miles 1790))

(defun tiles-to-miles (tiles)
  (floor (/ tiles 1790)))

(defun memoize (fn)
  "ty pg"
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
            val
            (setf (gethash args cache)
              (apply fn args)))))))

(defun compose (&rest fns)
  "ty pg"
  (if fns
    (let ((fn1 (car (last fns)))
           (fns (butlast fns)))
      #'(lambda (&rest args)
          (reduce #'funcall fns
            :from-end t
            :initial-value (apply fn1 args))))
    #'identity))

(defun enumerate (lst)
  (let ((n -1))
    (map 'list
      #'(lambda (item)
          (cons (incf n) item))
      lst)))
