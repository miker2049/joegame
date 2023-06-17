(defpackage config (:use :cl)
  (:export *db-path*
    *area-set*
    *area-def-table*
    *area-table*
    *conf-output*))
(in-package config)

(defparameter *db-path* "world.db")

(defparameter *area-set* '(("ocean" #xB7C4CF)
                            ("dirt" #x967E76)
                            ("grass" #xA0D8B3)
                            ("deep-grass" #xA2A378)
                            ("sand" #xEEE3CB)
                            ("hard-sand" #xD7C0AE)
                            ("stone" #xD6E8DB)
                            ("cliff" #x000000)))

(defparameter *area-def-table*
  "CREATE TABLE IF NOT EXISTS terrain (
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL,
  color INTEGER NOT NULL)")

(defparameter *area-table*
  "CREATE TABLE area (
  terrain_id INTEGER NOT NULL,
  x INTEGER NOT NULL,
  y INTEGER NOT NULL,
  FOREIGN KEY (terrain_id) REFERENCES terrain (id)
  UNIQUE(x, y, alt)
)")

(defparameter *conf-output* "worldconf.json")
