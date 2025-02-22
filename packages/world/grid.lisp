(defpackage grid (:use :cl :alexandria)
            (:export
             add-chunk
             chunk-list-to-grid
             get-height
             get-width
             get-sub-arr
             make-grid
             scale-grid
             print-grid
             make-empty-grid
             iterate-grid
             iterate-grid-values
             map-grid
             map-grid-values
             unique-grid-items
             grid-to-list
             flatten-grid
             clone-grid
             set-val
             at
             @
             xyi
             ixy
             grid-empty
             +grid))
(in-package grid)

(defun xyi (x y width)
  (+ x (* y width)))

(defun ixy (i width)
  (let ((x (mod i width))
        (y (floor (/ i width))))
    (values x y)))

(defun make-grid (w h l)
  (make-array (list h w) :initial-contents l))

(defun make-empty-grid (w h v)
  (let ((l
          (loop for yy from 0 below h collect
                                      (loop for xx from 0 below w collect v))))
    (make-grid w h l)))

(defun chunk-list-to-grid (l row-width)
  "Given a list and row-width, construct a grid from a list.  If it doesnt
go into the grid evenly, it trims from the list."
  (let* ((trimmed
           (subseq l 0 (- (length l) (mod (length l) row-width))))
         (out
           (make-array `(,(floor (/ (length trimmed) row-width)) ,row-width))))
    (loop
      :for idx
        :from 0
          :below (length trimmed)
      :do (multiple-value-bind (x y) (ixy idx row-width)
            (setf (aref out y x) (nth idx trimmed))))
    out))

(defun flatten-grid (grid)
  (loop for i below (array-dimension grid 0)
        append (loop for j below (array-dimension grid 1)
                     collect (aref grid i j))))

(defun grid-to-list (grid)
  (loop for i below (array-dimension grid 0)
        collect (loop for j below (array-dimension grid 1)
                      collect (aref grid i j))))

(defun make-grid-from-list (w h l)
  (make-array (list h w) :initial-contents l))

(defun at (g x y)
  (aref g y x))
(defun @ (g x y)
    (funcall #'at g x y))
(defun set-val (g v x y)
  (setf (aref g y x) v))
(defun get-width (g)
  (array-dimension g 1))
(defun get-height (g)
  (array-dimension g 0))
(defun get-row (arr row)
  (make-array (array-dimension arr 1)
              :displaced-to arr
              :displaced-index-offset (* row (array-dimension arr 1))))
(defun clone-grid (g) (copy-array g))

(defun iterate-grid (g cb)
  "Run a function for each coord of a grid,
callback expects (x y) coordinates."
  (loop :for yy :from 0 :below (get-height g)
        :do (loop :for xx :from 0 :below (get-width g)
                  :do (funcall cb xx yy))))

(defun iterate-grid-values (g cb)
  "Run a function for each value of a grid."
  (iterate-grid g #'(lambda (x y)
                      (funcall cb
                               (@ g x y)))))

(defun grids-same (g1 g2)
  (block iterloop
    (iterate-grid g1 #'(lambda (x y)
                         (let ((v1 (at g1 x y))
                               (v2 (at g2 x y)))
                           (if (not (eq v1 v2))
                               (return-from iterloop nil)))))
    t))
(defun grid-empty (g &optional empty)
  (block iterloop
    (iterate-grid g #'(lambda (x y)
                        (let ((v (at g x y)))
                          (if (not (equal (or empty nil) v))
                              (return-from iterloop nil)))))
    t))

(defun get-grid-data (g)
  (let ((l (list)))
    (iterate-grid g #'(lambda (x y)
                        (setf l
                              (append l (list (at g x y))))))
    l))


(defun inject-chunk (g ol xo yo)
  (iterate-grid ol #'(lambda (x y)
                       (let ((fx (+ x xo))
                             (fy (+ y yo)))
                         (if (or (> fy (get-height g))
                                 (> fx (get-width g)))
                             (error "Trying to inject into nonexistent spot.")
                             (set-val g (at ol x y) (+ x xo) (+ y yo))))))
  g)

(defun attach-chunk-output-dimension (gdim oldim offset)
  (if (< offset 0)
      (max (+ (abs offset) gdim) oldim)
      (max gdim (+ offset oldim))))

(defun attach-chunk (g ol xo yo &optional default)
  (let ((w (attach-chunk-output-dimension
            (get-width g)
            (get-width ol)
            xo))
        (h (attach-chunk-output-dimension
            (get-height g)
            (get-height ol)
            yo))
        (baseXo (if (< xo 0) (abs xo) 0))
        (baseYo (if (< yo 0) (abs yo) 0))
        (olXo (if (< xo 0) 0 xo ))
        (olYo (if (< yo 0) 0 yo )))
    (let ((out (make-empty-grid w h (or default 0))))
      (inject-chunk out g baseXo baseYo)
      (inject-chunk out ol olXo olYo)
      out)))

(defun add-chunk (g ol xo yo &optional default)
  (if (or (< xo 0) (< yo 0))
      (attach-chunk g ol xo yo default)
      (if (or (< (get-height g) (+ (get-height ol) yo))
              (< (get-width g) (+ (get-width ol) xo)))
          (attach-chunk g ol xo yo default)
          (inject-chunk g ol xo yo))))

(defun print-grid (g)
  (with-output-to-string (out)
    (format out "~%")
    (loop for y from 0 to (- (get-height g) 1)
          do (progn
               (loop for x from 0 to (- (get-width g) 1)
                     do (format out "~S" (at g x y)))
               (format out "~%")))
    out))

(defun get-sub-arr (x y w h g)
  (let ((out (make-empty-grid w h 0)))
    (iterate-grid out #'(lambda (xx yy)
                          (set-val out (at g (+ xx x) (+ yy y)) xx yy)))
    out))

(defun grid-from-grid (g)
  (copy-array g))

(defun get-center-grid (g)
  (values
   (floor (/ (get-width g) 2))
   (floor (/ (get-height g) 2))))

(defun grid-pad-row (g amount &optional v bottom)
  (let ((newrows (make-empty-grid (get-width g) amount (or v 0))))
    (add-chunk g newrows 0 (if bottom (get-height g) (* -1 amount)) v)))

(defun grid-pad-col (g amount &optional v left)
  (let ((newcols (make-empty-grid amount (get-height g) (or v 0))))
    (add-chunk g newcols (if left  (* -1 amount) (get-width g)) 0 v)))

(defun map-grid (g cb)
  "Map grid, callback expects (x y) args. "
  (iterate-grid g #'(lambda (x y)
                      (set-val g (funcall cb x y) x y)))
  g)

(defun map-grid-values (g cb)
  "Map grid, callback expects the current value of the grid"
  (map-grid g #'(lambda (x y)
                  (funcall cb
                           (@ g x y)))))

(defun scaled-xy (g scale x y)
  (at g
      (max (floor (/ x scale)) 0)
      (max (floor (/ y scale)) 0)))


(defun scale-grid (g scale)
  (let ((ow (floor (* (get-width g) scale)))
        (oh (floor (* (get-height g) scale))))
    (if (or (<= ow 0) (<= oh 0))
        (error "scale grid failed")
        (let ((out (make-empty-grid ow oh 0)))
          (map-grid out #'(lambda (x y)
                            (scaled-xy g scale x y)))))))

(defun encode-grid (g check)
  (let ((out 0))
    (iterate-grid g #'(lambda (x y)
                        (setf out
                              (if (eq check (at g x y))
                                  (logior (ash out 1) 1)
                                  (logior (ash out 1) 0)))))
    out))

(defun +grid (g i)
  (map-grid g
            #'(lambda (x y) (+ (@ g x y) i))))


(defun unique-grid-items (g &key (test #'eql))
  (let (out)
    (iterate-grid g
                  #'(lambda (x y)
                      (let ((item (@ g x y)))
                        (if (eql item 0) nil
                            (if (find item out :test test)
                                nil
                                (push item out))))))
    out))

(defun same-dimensions (g1 g2)
  (and (eql (grid-width g1) (grid-width g2))
       (eql (grid-height g1) (grid-height g2))))

(defun intersect-grids (g1 g2 &key (test (lambda (it) (unless (eql it 0) t))))
  "Returns a bit grid of where 1 marks where test is true
for both g1 and g2.  Will error if grids are not the same dimensions"
  (if (same-dimensions g1 g2)
      (let ((out-grid (clone-grid g1)))
        (iterate-grid out-grid
                      (lambda (x y)
                        (if (and (funcall test (@ g1 x y))
                                 (funcall test (@ g2 x y)))
                            (set-val out-grid 1 x y)
                            (set-val out-grid 0 x y)))))
      (error "Not same dimensions.")))


(defun replace-from-bit-grid (g bitg &key (replacement 0))
  (iterate-grid
   bitg (lambda (x y)
          (if (eql 1 (@ bitg x y))
              (set-val g replacement x y)))))
