(defpackage async (:use
                    :cl
                    :as
                    :bb
                    :bt)
  (:export await promise-all))
(in-package async)

(defmacro await (operation)
  "Run `operation` in a background thread, and resolve the returned promise with
   the result(s) of the operation once complete. The promise will be resolved on
   the same thread `(work ...)` was spawned from (your event-loop thread)."
  `(bb:with-promise (resolve reject :resolve-fn resolver)
    (let* ((err nil)
           (result nil)
           (notifier (as:make-notifier (lambda ()
                                         (if err
                                             (reject err)
                                             (apply resolver result))))))
      (bt:make-thread (lambda ()
                        (handler-case
                            (setf result (multiple-value-list (funcall (lambda () ,operation))))
                          (t (e) (setf err e)))
                        (as:trigger-notifier notifier))))))

(defmacro promise-all (promise-list cb)
  `(as:with-event-loop (:catch-app-errors t)
     ;; (as:with-delay (1) (format t "event loop still running...~%"))
     (bb:attach
       (bb:catcher
         (bb:all (bb:promisify ,promise-list))
         (t (e) (format t "err: ~a~%" e)))
       #',cb)))
