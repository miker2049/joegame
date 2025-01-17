(in-package #:worldtool)

(define-constant +window-width+ 800)
(define-constant +window-height+ 600)

(define-constant +repl-update-interval+ 0.3d0)

(defvar *resources-path*
  (asdf:system-relative-pathname :worldtool #P"Resources/"))

(deploy:define-hook (:boot set-resources-path) ()
  (setf *resources-path*
        (merge-pathnames #P"Resources/"
                         (uiop:pathname-parent-directory-pathname
                          (deploy:runtime-directory)))))

(define-constant +font-path+ "inconsolata.ttf" :test #'string=)
(define-constant +font-size+ 24)

(define-constant +config-path+ "../config.cfg" :test #'string=)

(defun init ()
  ;; TODO : put your initialization logic here
  )

(declaim (type fixnum *fps*))
(defvar *fps* 0)

(defun update (dt)
  (unless (zerop dt)
    (setf *fps* (round 1 dt)))

  ;; TODO : put your game logic here
  )

(defvar *font*)

(defun render ()

  (al:clear-to-color (al:map-rgb 221 0 2))
  (al:draw-text *font* (al:map-rgba 255 255 255 0) 0 0 0
                (format nil "~d FPS" *fps*))

  ;; TODO : put your drawing code here
  )

(cffi:defcallback %main :int ((argc :int) (argv :pointer))
  (declare (ignore argc argv))
  (handler-bind
      ((error #'(lambda (e) (unless *debugger-hook*
                              (al:show-native-message-box
                               (cffi:null-pointer) "Hey guys"
                               "We got a big error here :("
                               (with-output-to-string (s)
                                 (uiop:print-condition-backtrace e :stream s))
                               (cffi:null-pointer) :error)))))
    (uiop:chdir (setf *default-pathname-defaults* *resources-path*))
    (al:set-app-name "worldtool")
    (unless (al:init)
      (error "Initializing liballegro failed"))
    (let ((config (al:load-config-file +config-path+)))
      (unless (cffi:null-pointer-p config)
        (al:merge-config-into (al:get-system-config) config)))
    (unless (al:init-primitives-addon)
      (error "Initializing primitives addon failed"))
    (print "primitives init")
    (unless (al:init-image-addon)
      (error "Initializing image addon failed"))
    (print "image init")
    (unless (al:init-font-addon)
      (error "Initializing liballegro font addon failed"))
    (print "font init")
    (unless (al:init-ttf-addon)
      (error "Initializing liballegro TTF addon failed"))
    (print "ttf init")
    (if (not (al:is-audio-installed))
        (unless (al:install-audio)
          (error "Intializing audio addon failed")))
    (print "audio install")
    ;; (al:reserve-samples 1)
    (unless (al:init-acodec-addon)
      (error "Initializing audio codec addon failed"))
    (print "codec install")
    (unless (al:restore-default-mixer)
      (error "Initializing default audio mixer failed"))
    (print "mixer")
    (let ((display (al:create-display +window-width+ +window-height+))
          (event-queue (al:create-event-queue)))
      (when (cffi:null-pointer-p display)
        (error "Initializing display failed"))
      (print "start doing")
      (al:inhibit-screensaver t)
      (al:set-window-title display "worldtool")
      (al:register-event-source event-queue
                                (al:get-display-event-source display))
      (al:install-keyboard)
      (al:register-event-source event-queue
                                (al:get-keyboard-event-source))
      (al:install-mouse)
      (al:register-event-source event-queue
                                (al:get-mouse-event-source))
      (unwind-protect
           (cffi:with-foreign-object (event '(:union al:event))
             (init)
             (#+darwin trivial-main-thread:call-in-main-thread #-darwin funcall
              #'livesupport:setup-lisp-repl)
             (loop
               :named main-game-loop
               :with *font* := (al:ensure-loaded #'al:load-ttf-font
                                                 +font-path+
                                                 (- +font-size+) 0)
               :with ticks :of-type double-float := (al:get-time)
               :with last-repl-update :of-type double-float := ticks
               :with dt :of-type double-float := 0d0
               :while (loop
                        :named event-loop
                        :while (al:get-next-event event-queue event)
                        :for type := (cffi:foreign-slot-value
                                      event '(:union al:event) 'al::type)
                        :always (progn
                                  (if (eq type :KEY-UP)
                                      (let ((key (cffi:foreign-slot-value
                                                  event '(:struct al:keyboard-event) 'al::keycode)))
                                        (print key)))
                                  (not (eq type :display-close)))
                        )
               :do (let ((new-ticks (al:get-time)))
                     (setf dt (- new-ticks ticks)
                           ticks new-ticks))
                   (when (> (- ticks last-repl-update)
                            +repl-update-interval+)
                     (livesupport:update-repl-link)
                     (setf last-repl-update ticks))
                   (al:clear-to-color (al:map-rgb 22 100 2))
                   (livesupport:continuable
                     (update dt)
                     (render))
                   (al:flip-display)
               :finally (progn (al:destroy-font *font*)

                               (al:inhibit-screensaver nil)
                               (print "dont inhibit")

                               (al:destroy-display display)
                               (print "destroy display")

                               (al:destroy-event-queue event-queue)
                               (print "event quere")

                               (al:stop-samples)
                               (print "stop samples")

                               ;; (al:destroy-mixer (al:get-default-mixer))
                               ;; (print "detach voice")
                               (trivial-garbage:gc :full t)
                               (if (al:is-audio-installed)
                                   (al:uninstall-audio))
                               (print "uninstall audio")
                               ;; (al:uninstall-system)
                               ;; (print "uninstall sys")
                               ;; (al:shutdown-ttf-addon)
                               ;; (print "shutdown ttf")

                               ;; (al:shutdown-font-addon)
                               ;; (print "shutdown font")

                               ;; (al:shutdown-image-addon)
                               ;; (print "shutdown image")
                               )))
        (print "donedone"))))
  0)

(defun main ()
  (#+darwin trivial-main-thread:with-body-in-main-thread #-darwin progn nil
            (float-features:with-float-traps-masked
                (:divide-by-zero :invalid :inexact :overflow :underflow)
              (al:run-main 0 (cffi:null-pointer) (cffi:callback %main)))))

(defclass window (al:system)
  ((previous-key :initform "Nothing" :accessor previous-key))
  (:default-initargs
   :title "Simple"
   :width 800 :height 600
   :logic-fps 1
   :display-flags '(:windowed :opengl :resizable)
   :display-options '((:sample-buffers 1 :suggest)
                      (:samples 4 :suggest))))

(defparameter +pos+ '(10 10))

(defmethod al:update ((sys window))
  (let ((dt (al:frame-time sys)))
    (unless (zerop dt)
      (setf *fps* (round 1 dt))))

  (print 'one-logic-frame))

(defmethod al:render ((sys window))
  (al:clear-to-color (al:map-rgb 20 150 100))
  (al:draw-filled-rectangle 10 10 20 20 (al:map-rgb 255 0 0))

  (al:draw-text *font* (al:map-rgba 255 255 255 0) 0 0 0
                (format nil "~d FPS" *fps*))
  (al:flip-display))

;; The lisp interface runs handlers during the logic step
;; Handlers are defined according to allegro events
(defmethod al:key-down-handler ((sys window))
  (let ((keyboard (cffi:mem-ref (al:event sys) '(:struct al:keyboard-event))))
    (print (getf keyboard 'al::keycode))
    (setf (previous-key sys) (getf keyboard 'al::keycode))))


(defun mainrun ()
  (let ((*font*  (al:ensure-loaded
                  #'al:load-ttf-font
                  +font-path+
                  (- +font-size+) 0)))

    (uiop:chdir *resources-path*)
    (al:run-system (make-instance 'window))))


(al:ensure-loaded
 #'al:load-ttf-font
 +font-path+
 (- +font-size+) 0)
