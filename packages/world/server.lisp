
(defpackage worldconf.server
  (:use
   :cl
   :worldconf)
  (:export start stop))
(in-package worldconf.server)

(defvar *app* (make-instance 'ningle:app))

(defun make-tile-stream (x y z)
  (flexi-streams:with-output-to-sequence (s)
    (png:encode
     (worldtiles:world-tile x y z)
     s)
    s))
(defvar make-tile-stream-memo (utils:memoize #'make-tile-stream))

(setf make-tile-stream-memo (utils:memoize #'make-tile-stream))

(setf (ningle:route *app* "/worldtile/:z/:x/:y")
      (lambda (params)
        `(200 (:content-type "image/png" :access-control-allow-origin  "*")
          ,(funcall make-tile-stream-memo
                    (parse-integer (cdr (assoc :x params)))
                    (parse-integer (cdr (assoc :y params)))
                    (parse-integer (cdr (assoc :z params)))))))


(setf (ningle:route *app* "/worldmap/:x/:y/:file/:rank" )
      (lambda (params)
        `(200 (:content-type "application/json" :access-control-allow-origin  "*")
          (,(jojo:to-json (worldconf:get-wang-serial worldconf:*worldconf*
                                                     (parse-integer (cdr (assoc :x params)))
                                                     (parse-integer (cdr (assoc :y params)))
                                                     (parse-integer (cdr (assoc :file params)))
                                                     (parse-integer (cdr (assoc :rank params)))))))))

(defvar *srv* nil)
(defun start (&optional port)
  (if (not *srv*)
      (setf *srv*
            (clack:clackup *app* :port (or port 5000)))
      (print "Already running")))

(defun stop ()
  (if *srv*
      (progn
        (clack:stop *srv*)
        (setf *srv* nil))
      (print "not running")))
