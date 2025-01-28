(defpackage sqlite-world
  (:use :common-lisp :cffi)
  (:export :init :insert* :finish*))

(in-package sqlite-world)

(define-foreign-library libsqlite-world
  (:unix  "libsqlite-world.so")
  (t (:default "libsqlite-world")))
(use-foreign-library libsqlite-world)

(defvar *bi* nil "The bulk-insert singleton instance")

(defcstruct bulk-insert
  (db (:pointer (:struct sqlite-ffi::sqlite3)))
  (stmt (:pointer (:struct sqlite-ffi::sqlite3-stmt))))

(defun init (path)
  (let ((bi (foreign-alloc '(:struct bulk-insert))))
    (foreign-funcall "bulk_insert_init"
                     :pointer bi
                     :string path
                     :uint16)
    (setf *bi* bi)
    bi))

(defun insert (bi idx val)
  (foreign-funcall "bulk_insert_row"
                   :pointer bi
                   :uint32 idx
                   :uint8 val
                   :uint8))

(defun insert* (idx val)
  (insert *bi* idx val))

(defun finish (bi)
  (foreign-funcall "bulk_insert_finish"
                   :pointer bi
                   :void))

(defun finish* ()
  (finish *bi*)
  (setf *bi* nil))
;; (defcfun bulk-insert-init ())
