(defpackage config (:use :cl)
  (:export *db-path*
    *area-set*
    *area-def-table*
    *area-table*
    *conf-output*))
(in-package config)

(defparameter *db-path* "world.db")


(string-downcase
  (symbol-name :depths))

(defun make-area-config (sym color id)
  `(,sym . (:id ,id
             :name ,(string-downcase
                      (symbol-name sym))
             :color ,color)))

(car
  (make-area-config :depths #x313e49 0))

(defparameter *area-set* (mapcar
                           #'(lambda (item)
                               ;; (print (cadr item))
                               ;; (print (cddr item))
                               ;; (print (cdar item))
                               (make-area-config
                                 (cadr item)
                                 (parse-integer
                                   (string-left-trim "#" (cddr item))
                                   :radix 16)
                                 (car item)))
                           (utils:enumerate
                             '((:depths . "#313e49")
                                (:trench . "#5c758a")
                                (:ocean . "#B7C4CF")
                                (:shore . "#e0b483")
                                (:late-shore . "#c69763")
                                (:coastal . "#c6ad74")
                                (:grass-and-sand . "#839450")
                                (:rocky-sand . "#B18E68")
                                (:desert . "#ffffd3")
                                (:desert-graveyard . "#faa06b")
                                (:dead-forest . "#f4c992")
                                (:old-pavement-desert . "#b89a74")
                                (:boulder-meadow-desert . "#96794d")
                                (:water-desert . "#c5e9bd")
                                (:field . "#33590e")
                                (:old-pavement-field . "#8f8f51")
                                (:forest . "#293b09")
                                (:forest-magic . "#2e4114")
                                (:water-forest . "#2e352e")
                                (:old-pavement-forest . "#444353")))))


(car
  (caddr (utils:enumerate
           '((:depths . #x313e49)
              (:trench . #x5c758a)
              (:ocean . #xB7C4CF)))))

(defparameter *tile-set* '((:ocean . (:id 0 :name "ocean" :color #xB7C4CF))
                            (:dirt . (:id 1 :name "dirt" :color #x967E76))
                            (:grass . (:id 2 :name "grass" :color #xA0D8B3))
                            (:deep-grass . (:id 3 :name "deep-grass" :color #xA2A378))
                            (:sand . (:id 4 :name "sand" :color #xEEE3CB))
                            (:hard-sand . (:id 5 :name "hard-sand" :color #xD7C0AE))
                            (:stone . (:id 6 :name "stone" :color #xD6E8DB))
                            (:cliff . (:id 7 :name "cliff" :color #x000000))
                            (:stone . (:id 8 :name "stone" :color #xF6F1F1))
                            (:ice . (:id 9 :name "ice" :color #xAFD3E2))
                            (:clay . (:id 10 :name "clay" :color #xC38154))))

(defparameter *area-def-table*
  "CREATE TABLE IF NOT EXISTS area_def (
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL,
  color INTEGER NOT NULL)")

(defparameter *area-table*
  "CREATE TABLE area (
  area_def_id INTEGER NOT NULL,
  x INTEGER NOT NULL,
  y INTEGER NOT NULL,
  FOREIGN KEY (area_def_id) REFERENCES area_def (id)
  UNIQUE(x, y)
)")

(defparameter *conf-output* "worldconf.json")
