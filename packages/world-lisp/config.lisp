(defpackage config (:use :cl)
  (:export *db-path*
    *area-set*
    *area-def-table*
    *area-table*
    *conf-output*))
(in-package config)

(defparameter *db-path* "world.db")

(defparameter *area-set* '((:ocean . (:id 0 :name "ocean" :color #xB7C4CF))
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
