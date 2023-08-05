(in-package :cl-user)
(defpackage tile-server.web
  (:import-from :cl-who
    with-html-output-to-string
    html-mode)
  (:use :cl
    :caveman2
    :tile-server.config
    :tile-server.view
    :tile-server.db
    :datafly
    :sxql)
  (:export :*web*))
(in-package :tile-server.web)

(setf (cl-who:html-mode) :HTML5)
;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Routing rules

(defun range (n)
  (loop for i from 0 to n collect i))

(defroute "/" ()
  (render #P"index.html" `(:rows-n ,(range (envy:config :tile-server.config :worldmap-size))
                            :img-n ,(range (envy:config :tile-server.config :worldmap-size)))))

(defroute "/terrain" ()
  (render #P"terrain.html"))

(defroute "/terrain" ()
  (render #P"terrain.html"))

(defroute "/get-terrain" ()
  (render #P"terrain.html"))


(defun get-binary-data (path)
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let ((data (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence data stream)
      data)))

(defroute "/image/:file" (&key file)
  (let ((d (get-binary-data (format nil "~a~a" "/home/mik/joegame/assets/images/" file))))
    `(200 (:content-type "image/png") ,d)))


(defroute "/imageplus/:file" (&key file)
  (let* ((p
           (png:decode-file
             (format nil "~a~a" "/home/mik/joegame/assets/images/" file)
             :preserve-alpha 't))
          (data (flexi-streams:with-output-to-sequence (s)
                  (png:encode
                    (render:extrude-tileset-image p
                      :margin 0
                      :spacing 0
                      :extrusion 1)
                    s))))
    `(200 (:content-type "image/png") ,data)))

(defroute "/worldtile/image/:row/:tile" (&key row tile)
  (with-html-output-to-string (output)
    (:div :class "clicked noselect"
      (:img :class "noselect"
        :draggable nil
        :src (format nil "/mwtiles/mw-~A-~A.png"
               (* 256 (parse-integer tile))
               (* 256 (parse-integer row)))))))
;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
    *template-directory*))
