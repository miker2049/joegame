(defpackage config (:use :cl)
  (:export *db-path*
    *terrain-set*
    *conf-output*))
(in-package config)

(defparameter *db-path* "world.db")

(defparameter *terrain-set* '(
                               ("ocean" #xB7C4CF)
                               ("dirt" #x967E76)
                               ("grass" #xA0D8B3)
                               ("deep-grass" #xA2A378)
                               ("sand" #xEEE3CB)
                               ("hard-sand" #xD7C0AE)
                               ("stone" #xD6E8DB)
                               ("cliff" #x000000)
                               ))

(defparameter *conf-output* "worldconf.json")
