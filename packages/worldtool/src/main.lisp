(in-package #:worldtool)

(declaim (type (vector (unsigned-byte 8) 227) *keys*))
(defvar *keys* (make-array 227 :element-type '(unsigned-byte 8)))
;;;;; inner
;;;;; https://github.com/liballeg/allegro_wiki/wiki/Allegro-Vivace-%E2%80%93-Input
;; 00000000 -- unpressed
;; 00000011 -- pressed
;; 00000010 -- released
;; 00000001 -- logic has run, key is still pressed
;; 00000000 -- logic has run, key is no longer pressed
(defun %handle-events (event event-type)
  (case event-type
    (:timer (progn
              (handle-events)
              (loop for idx below +key-max+ do
                (setf  (aref *keys* idx)
                       (logand (aref *keys* idx)
                               +key-seen-flag+)))))
    (:key-down (setf (aref *keys*
                           (key
                            (cffi:foreign-slot-value
                             event
                             '(:struct al:keyboard-event)
                             'al::keycode)))
                     (logior +key-seen-flag+
                             +key-released-flag+)))
    (:key-up (let ((kc (cffi:foreign-slot-value
                        event
                        '(:struct al:keyboard-event)
                        'al::keycode)))
               (setf (aref *keys* (key kc))
                     (logand (aref *keys* (key kc))
                             +key-released-flag+))))))

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
    (al:set-app-name "worldtool2")
    (unless (al:init)
      (error "Initializing liballegro failed"))
    (let ((config (al:load-config-file +config-path+)))
      (unless (cffi:null-pointer-p config)
        (al:merge-config-into (al:get-system-config) config)))
    (unless (al:init-primitives-addon)
      (error "Initializing primitives addon failed"))
    (unless (al:init-image-addon)
      (error "Initializing image addon failed"))
    (unless (al:init-font-addon)
      (error "Initializing liballegro font addon failed"))
    (unless (al:init-ttf-addon)
      (error "Initializing liballegro TTF addon failed"))
    (if --enable-sound
        (progn
          (unless (al:install-audio)
            (error "Intializing audio addon failed"))
          (unless (al:init-acodec-addon)
            (error "Initializing audio codec addon failed"))
          (unless (al:restore-default-mixer)
            (error "Initializing default audio mixer failed"))))
    (let ((display (al:create-display +window-width+ +window-height+))
          (timer (al:create-timer (/ 1.0 30.0)))
          (event-queue (al:create-event-queue)))
      (unless timer
        (error "No timer"))
      (when (cffi:null-pointer-p display)
        (error "Initializing display failed"))
      (al:inhibit-screensaver t)
      (al:set-window-title display "worldtool2")
      (al:register-event-source event-queue
                                (al:get-display-event-source display))
      (al:install-keyboard)
      (al:register-event-source event-queue
                                (al:get-keyboard-event-source))
      (al:install-mouse)
      (al:register-event-source event-queue
                                (al:get-mouse-event-source))
      (al:register-event-source event-queue
                                (al:get-timer-event-source timer))
      (unwind-protect
           (cffi:with-foreign-object (event '(:union al:event))
             (init)
             (al:start-timer timer)
             (#+darwin trivial-main-thread:call-in-main-thread #-darwin funcall
              #'livesupport:setup-lisp-repl)
             (loop
               :named main-game-loop
               :with ticks :of-type double-float := (al:get-time)
               :with last-repl-update :of-type double-float := ticks
               :with dt :of-type double-float := 0d0
               :while (loop
                        :named event-loop
                        :while (al:get-next-event event-queue event)
                        :for type := (cffi:foreign-slot-value
                                      event '(:union al:event) 'al::type)
                        :always (progn
                                  (%handle-events event type)
                                  (not (eq type :display-close))))
               :do (let ((new-ticks (al:get-time)))
                     (setf dt (- new-ticks ticks)
                           ticks new-ticks))
                   (when (> (- ticks last-repl-update)
                            +repl-update-interval+)
                     (livesupport:update-repl-link)
                     (setf last-repl-update ticks))
                   (al:clear-to-color (al:map-rgb 0 0 0))
                   (livesupport:continuable
                     (update dt)
                     (render))
                   (al:flip-display)
               :finally (shutdown)))
        (al:inhibit-screensaver nil)
        (al:destroy-timer timer)
        (al:destroy-event-queue event-queue)
        (al:destroy-display display)
        (if --enable-sound
            (al:stop-samples))
        (al:uninstall-system)
        (if --enable-sound
            (al:uninstall-audio))
        (al:shutdown-ttf-addon)
        (al:shutdown-font-addon)
        (al:shutdown-image-addon))))
  0)



