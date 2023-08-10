(defpackage utils (:use :cl :alexandria)
  (:export
    enumerate
    e-distance
    memoize
    miles-to-tiles
    tiles-to-miles
    tile-n
    define-serializable
    get-json-from-serializable))

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
  (let ((cache (make-hash-table :synchronized t :test #'eql :weakness :key-or-value :rehash-size *memo-cache-limit*)))
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
            val
            (let ((nv (apply fn args)))
              (setf (gethash args cache) nv)
              nv))))))



(defun enumerate (lst)
  (let ((n -1))
    (map 'list
      #'(lambda (item)
          (cons (incf n) item))
      lst)))


(defun tile-n (tile-h tile-w width)
  (loop
    :for row
    :from 0
    :to (- width 1)
    :collect
    (loop
      :for tile
      :from 0
      :to (- width 1)
      :collect
      `( :x ,(* tile tile-w)
         :y ,(* row tile-h)
         :width ,tile-w
         :height ,tile-h))))


;; Helper class

(defmacro define-class-from-spec (class-name supers &body slots)
  `(defclass ,class-name ,supers
     ,(mapcar (lambda (slot)
                `(,(car slot)
                   :initarg ,(intern (string-upcase (symbol-name (car slot))) :keyword)
                   :initform ,(cadr slot)
                   :accessor ,(car slot)))
        slots)))


(defmacro define-json-method (class-name &body slots)
  `(defmethod jojo:%to-json ((obj ,class-name))
     (jojo:with-object
       ,@(mapcar (lambda (slot)
                   `(jojo:write-key-value
                      ,(string-downcase
                         (if (eql (length slot) 3)
                           (nth 2 slot)
                           (symbol-name (car slot))))
                      (slot-value obj ',(car slot))))
           slots))))
(defgeneric to-plist (obj)
  (:method-combination nconc))

(defmacro define-plist-serialize (class-name &body slots)
  `(defmethod to-plist nconc ((obj ,class-name))
     (list ,@(mapcan (lambda (slot)
                       `(,(intern
                            (string-downcase
                              (if (eql (length slot) 3)
                                (nth 2 slot)
                                (symbol-name (car slot))))
                            "KEYWORD")
                          (slot-value obj ',(car slot))))
               slots))))

(defmacro define-deserialization (class-name &body slots)
  `(defun ,(intern (string-upcase
                     (format nil "deserialize-~a" class-name)))
     (obj)
     "Expects a plist, like one returned from jojo:parse"
     (make-instance ',class-name
       ,@(mapcan
           (lambda (slot)
             (let ((property
                     (if (eql (length slot) 3)
                       (nth 2 slot)
                       (symbol-name (car slot)))))
               (list (intern (symbol-name (car slot))
                       "KEYWORD")
                 `(getf obj ,(intern
                               (string-downcase property)
                               "KEYWORD")))))
           slots))))

(defun deduplicate-keys (plist)
  "Remove duplicate keys from a plist."
  (let ((result '())
         (seen (make-hash-table :test 'equal)))
    (loop for (key value) on plist by #'cddr do
      (when (not (gethash key seen))
        (setf (gethash key seen) t)
        (push key result)
        (push value result)))
    (nreverse result)))

(defgeneric serialize (obj)
  (:method (obj)
    obj)
  (:documentation
    "Get Json string"))

(defmethod get-json-from-serializable ((obj list))
  (jojo:to-json (mapcan #'get-json-from-serializable obj)))

(defmacro define-serializable (class-name supers &body slot-spec)
  `(progn
     (define-class-from-spec ,class-name ,supers ,@slot-spec)
     (define-plist-serialize ,class-name ,@slot-spec)
     (define-deserialization ,class-name ,@slot-spec)
     (defmethod serialize ((obj ,class-name))
       (jojo:to-json (deduplicate-keys (to-plist obj))))))
