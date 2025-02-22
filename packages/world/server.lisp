
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


(defun get-map-data* (x y file rank)
  (let* ((xx (+ (* 256 x) (* 32 file)))
         (yy (+ (* 256 y) (* 32 rank)))
         (data (worldconf:get-map-data worldconf:*worldconf*
                                       xx
                                       yy
                                       33
                                       33))

         (objs (loop for lay in (getf data :object-masks)
                     append (worldconf.csp:get-objects
                             (getf lay :mask)
                             (getf lay :name)
                             (worldconf:cantor xx yy))))
         (chars  (loop for lay in (getf data :object-masks)
                       append
                       (list (intern (getf lay :name) 'keyword)
                             (mapcar #'symbol-name
                                     (worldconf.csp:get-chars
                                      (getf lay :name)
                                      (worldconf:cantor xx yy)
                                      3))))))
    (list
     :|wang| (getf data :wang)
     :|chars| chars
     :|objects| objs )))


(setf (ningle:route *app* "/worldmap/:x/:y/:file/:rank" )
      (lambda (params)
        `(200 (:content-type "application/json" :access-control-allow-origin  "*")
          (,(jojo:to-json (get-map-data*
                           (parse-integer (cdr (assoc :x params)))
                           (parse-integer (cdr (assoc :y params)))
                           (parse-integer (cdr (assoc :file params)))
                           (parse-integer (cdr (assoc :rank params)))))))))

(setf (ningle:route *app* "/assets/*" )
      (lambda (params)
        (let ((path (cadr (assoc :splat params))))
          (if (not (uiop:file-exists-p (joegame-assets:get-asset-path path)))
              '(404 () "nothing")
              `(200 (:content-type "image/png" :access-control-allow-origin  "*")
                ,(alexandria:read-file-into-byte-vector (joegame-assets:get-asset-path path)))))))



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

(symbol-name :|hey|)
