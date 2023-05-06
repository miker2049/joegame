(ql:quickload 'jonathan)
(ql:quickload 'alexandria)
(defpackage "worldconf" (:use :cl :jonathan :alexandria))
(in-package "worldconf")

;utilities;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun downcase (s)
    (map 'string #'(lambda (char) (char-downcase char))
        s))


(defun q2s (q) (downcase (string q)))

(defun j (plist)
    "Convert plist to json"
    (downcase
        (to-json plist)))

(defun sig (type &optional (params '()) (filters '()) (children '()))
    "Generate serialized sig.  A signal always has children, which are either
other signals, or terminating terrain layers. In the case of only two children,
 the split value defines how the children are placed. Otherwise, the signal is segmented
proportionally (like classic cliff system)."
    `(:type "signal" :name ,type :params ,params :filters ,filters :children ,children))

(defun filter (type &optional (params '()))
    "A filter processed the value of a signal"
    `(:type ,type :params ,params))

(defun fbinary (n)
    (filter "binary" (list (list "n" n))))

(defun terrain (name &key color file children)
    "Terrains can be terminating, but if they have children,
they are run through in order as usual.  On a given quad, ff another terrain
is a child of this terrain and visible, it overwrites this one"
    (list :type "terrain"
        :file (or file "dummy.json")
        :color (or color "black")
        :children (or children '())
        :name name))


(defun perlin (&key freq seed children)
    (sig "perlin" `(("freq" ,freq)
                       ("depth" 7)
                       ("seed" ,(or seed 0)))
        '() children))

(defun filler (&rest children) (sig "fill" :children children))

(defmacro make-terrain (name &optional color file)
    `(progn
         (defun ,name (&rest children)
               (terrain ,(symbol-name name) :color ,color :file ,file
                   :children children))
         (setq ,name (,name))))



(make-terrain ocean "blue")
(make-terrain dirt "brown")
(make-terrain grass "green")
(make-terrain deep-grass "darkgreen")
(make-terrain sand "yellow")
(make-terrain hard-sand "darkyellow")
(make-terrain stone "gray")



;pieces;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun ocean-water ()
    (terrain "water"))

(defun land (&rest children)
    (perlin :freq 0.009 :seed 108 :children children))


(defun lowland (&optional seed)
    "A perlin generator within a perlin generator, mixing deep grasses"
    (dirt
        (perlin :freq 0.01 :seed seed
            :children (list 'stone
                        (perlin :freq 0.1 :seed 420
                            :children '(grass deep-grass))))))

(defun desert (&optional seed)
    "A perlin generator within a perlin generator, mixing deep grasses"
    (dirt
        (perlin :freq 0.01 :seed seed
            :children (list (perlin :freq 0.1 :seed 420
                            :children '(sand hard-sand))))))

(defun forest (&optional seed)
    "A perlin generator within a perlin generator, mixing deep grasses"
    (grass
        (perlin :freq 0.01 :seed seed
            :children (list (perlin :freq 0.1 :seed 67433
                            :children '(deep-grass))))))
(defvar finalconf '()
    "final conf thats turned to json")

(defun t-obj (name &optional n)
    "A terrain object"
    (list :name name :n (or n 0.05)))



(setq finalconf
        (ocean
            (land
                (lowland)
                (lowland)
                (lowland)
                (desert)
                (desert)
                (desert)
                (forest)
                (forest)
                (forest)
                (forest))))

(defvar confpath "worldconf.json")


(if (probe-file confpath)
    (delete-file confpath))
(write-string-into-file
    (j finalconf)
     confpath :if-exists :overwrite
    :if-does-not-exist :create)
