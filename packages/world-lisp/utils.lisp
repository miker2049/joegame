(defpackage utils (:use :cl)
  (:export
    enumerate
    e-distance
    memoize
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

(defun delete-n-hashtable (table n)
  (let ((keys '()))
    (with-hash-table-iterator (hiter table)
      (loop
        (multiple-value-bind (entry-p key value)
          (hiter)
          (if entry-p
            (progn
              (setf keys
                (nconc keys (list key)))
              (if (>= (length keys) n)
                (return)))
            (return)))))
    (dolist (key keys)
      (remhash key table))))

(defvar *memo-cache-limit* 1000000)
(defun memoize (fn)
  "ty pg. With a very dirty limiter in size."
  (let ((lock (bt:make-lock))
         (cache (make-hash-table :test #'equal :rehash-size *memo-cache-limit*)))
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
            val
            (progn
              (if (> (hash-table-count cache) *memo-cache-limit*)
                (progn
                  (bt:acquire-lock lock)
                  (delete-n-hashtable cache
                    *memo-cache-limit*)
                  (bt:release-lock lock)))
              (let ((nv (apply fn args)))
                (bt:acquire-lock lock)
                (setf (gethash args cache) nv)
                (bt:release-lock lock)
                nv)))))))


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
