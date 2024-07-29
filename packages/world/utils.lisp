(defpackage utils (:use :cl :alexandria)
            (:export
             decode-png-file
             split
             concat-lists
             range
             find-files
             mkdir
             parse-html-hex-string
             lazy-generated-file
             make-lgf
             move-lgf
             gen-fun
             get-lgf
             lgf-path
             lgf-filep
             clean-lgf
             fmt
             cp
             symlink
             s+
             mktemp
             mktempd
             define-deserialization
             to-plist
             save-file
             save-file-bytes
             enumerate
             e-distance
             memoize
             miles-to-tiles
             tiles-to-miles
             tile-n
             define-serializable
             serialize
             pathname-no-extension
             get-json-from-serializable
             fn
             fuzzmatch
             filter
             take
             sha256
             sha256-file))

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
  (let ((cache (make-hash-table :synchronized t :test #'equal :weakness :key-or-value :rehash-size *memo-cache-limit*)))
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
;; (defmethod to-plist nconc ((obj null)))
;; (defmethod to-plist nconc ((obj cons))
;;   (nconc (list (to-plist (car obj)))
;;          (to-plist (cdr obj))))

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
     (defparameter ,(symbolicate (format nil "~a-slots" class-name)) ',slot-spec)
     (define-class-from-spec ,class-name ,supers ,@slot-spec)
     (define-plist-serialize ,class-name ,@slot-spec)
     (define-deserialization ,class-name ,@slot-spec)
     (defmethod serialize ((obj ,class-name))
       (jojo:to-json (deduplicate-keys (to-plist obj))))))






(defun save-file (path b)
  (with-open-file (s path
                     :direction :output
                     :if-exists :supersede)
    (format s "~a" b)))

(defun save-file-bytes (path b)
  (alexandria:write-byte-vector-into-file b path
                                          :if-exists :supersede))

(defmacro def-unix (command &key n-args func-name)
  (let ((args (if n-args
                  (mapcar #'gensym (loop :for idx :below n-args :collect idx))
                  nil)))
    `(defun ,(intern (string-upcase (or func-name command))) (,@args)
       (string-trim '(#\Newline)
                    (multiple-value-bind (output error-output exit-code)
                        (uiop:run-program (list ,command ,@args) :output 'string)
                      output)))))

(def-unix "mktemp")
(def-unix "mktemp" :n-args 1 :func-name "mktemp-1")
(defun mktempd ()
  (mktemp-1 "-d"))
(def-unix "ls" :n-args 1)
(def-unix "mv" :n-args 3 :func-name "mv-3")
(defun mv (a b)
  (mv-3 "-f" a b))
(def-unix "ls" :n-args 0 :func-name "lscwd")
(def-unix "cp" :n-args 3 :func-name "cp-3")
(def-unix "mkdir" :n-args 2 :func-name "mkdir-2")

(def-unix "find" :n-args 5 :func-name "unixfind")

(defun split-string-lines (str)
  (uiop:split-string str :separator '(#\linefeed)))

(defun find-files (dir search)
  "Search dir recursively for search term and get back just files."
  (split-string-lines
   (unixfind dir "-iname" search "-type" "f")))


;; (find-files  "/home/mik/joegame/assets/images" "*.png")

;; (concatenate)

(defun sha256-file (file)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-file :sha256 file)))

(defun sha256 (coll)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha256 coll)))

(defun cp (src dst)
  (cp-3 "-ru" src dst))

(def-unix "ln" :n-args 3 :func-name "ln-3")
(defun symlink (source target)
  "The source is what the target will point to."
  (ln-3 "-s" source target))

(defun mkdir (path)
  (mkdir-2 "-p" path))

(defun s+ (s1 s2)
  (concatenate 'string s1 s2))

(defun pathname-no-extension (path)
  (merge-pathnames
   (directory-namestring path)
   (pathname-name path)))

(defclass lazy-generated-file ()
  ((outpath
    :initarg :outpath
    :accessor lgf-path)
   (gen-fun
    :initarg :gen-fun
    :accessor gen-fun)))

(defun make-lgf (outpath func)
  (make-instance 'lazy-generated-file :outpath outpath :gen-fun func))

(defmethod run-lgf ((lgf lazy-generated-file))
  (funcall (gen-fun lgf) lgf))

(defmethod set-lgf-out ((lgf lazy-generated-file) (path string))
  (setf (lgf-path lgf) path))

(defmethod set-lgf-dir ((lgf lazy-generated-file) (path string))
  (if (uiop:directory-exists-p path)
      (set-lgf-out lgf
                   (merge-pathnames path
                                    (file-namestring
                                     (lgf-path lgf))))
      (error (fmt "Directory ~a doesn't exist for lgf file ~a"
                  path
                  (file-namestring (lgf-path lgf))))))

(defmethod move-lgf ((lgf lazy-generated-file) (path string))
  (unless (string= path (lgf-path lgf))
    (if (lgf-filep lgf)
        (progn
          (mv (lgf-path lgf) path)
          (set-lgf-out lgf path))
        (set-lgf-out lgf path))))

(defmethod lgf-filep ((lgf lazy-generated-file))
  (uiop:file-exists-p (lgf-path lgf)))

(defgeneric clean-lgf (it)
  (:method (it) it))
(defmethod clean-lgf ((lgf lazy-generated-file))
  (uiop:delete-file-if-exists (lgf-path lgf)))


(defmethod get-lgf ((lgf lazy-generated-file))
  (if (lgf-filep lgf)
      (lgf-path lgf)
      (progn
        (run-lgf lgf)
        (if (not (lgf-filep lgf))
            (error "lgf function does not create its file")
            (lgf-path lgf)))))



(defmacro fmt (fstr &rest args)
  `(format nil ,fstr ,@args))

(defun parse-html-hex-string (str)
  "Parse strings like \"#ffef04\" to their integer form"
  (parse-integer (string-left-trim `(#\#) str) :radix 16))


(defun levenshtein-distance (search target)
  "Compute the Levenshtein distance between strings search and t."
                                        ; Base case: empty strings
  (if (or (string= search "") (string= target ""))
      (max (length search) (length target))
                                        ; Case when characters are equal
      (if (char= (char search 0) (char target 0))
          (levenshtein-distance (subseq search 1) (subseq target 1))
                                        ; Not equal characters
          (1+ (min
                                        ; Delete from search
               (levenshtein-distance (subseq search 1) target)
                                        ; Delete from target
               (levenshtein-distance search (subseq target 1))
                                        ; Delete from both
               (levenshtein-distance (subseq search 1) (subseq target 1)))))))



(defun fuzzmatch2 (input target)
  (let ((fullmatch
          (if (ppcre:scan input target)
              25 0))
        (fmatch
          (if (ppcre:scan (fmt ".*~a.*"
                               (ppcre:regex-replace-all " " input ".*"))
                          target)
              10 0))
        (word-score
          (length
           (ppcre:all-matches
            (fmt "(~a)+"
                 (ppcre:regex-replace-all " " input "|"))
            target))))
    (+ fullmatch fmatch word-score)))


(defmacro fn (&body body)
  (let ((it (gensym)))
    `(lambda (it)
       ,@body)))

(defmacro fn2 (&body body)
  `(lambda (ita itb)
     ,@body))


(defun fuzzmatch (inp targ)
  (ppcre:scan
   (apply #'concatenate
          (cons 'string
                (cons ".*"
                      (mapcar (fn (fmt "~a.*" it))
                              (uiop:split-string inp)))))
   targ))



(defun range (n &key (start 0))
  (loop for i from start below n collect i))

(defmacro concat-lists (&body lists)
  `(concatenate 'list ,@lists))

(defun filter (elements test)
  (loop
    for e in elements
    when (funcall test e)
      collect e))

(defun take (n col)
  (loop for it below n collect (nth it col)))

(defun sassoc (item alist)
  "String association lists."
  (cdr
   (assoc item alist :test #'string=)))

(defun split (somestring &key delim)
  "Simple function, similar to String.prototype.split in JavaScript"
  (let ((s (list )) (current ""))
    (loop for char across somestring do
      (if (equal delim char)
          (progn
            (setq s (concatenate 'list s (list current)))
            (setq current ""))
          (setq current (concatenate 'string current (string char)))))
    (remove-if #'(lambda (item) (equal "" item)) (concatenate 'list s (list current)))))


(defun decode-png-file (pathname &key swapbgr preserve-alpha)
  (with-open-file (input pathname :element-type '(unsigned-byte 8))
    (png:decode input :swapbgr swapbgr :preserve-alpha preserve-alpha)))
