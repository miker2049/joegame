;;; jat.el --- Tool for manipulating joegame asset -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Mike Russo
;;
;; Author: Mike Russo <russomichaelb@gmail.com>
;; Maintainer: Mike Russo <russomichaelb@gmail.com>
;; Created: January 27, 2023
;; Modified: January 27, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/mik/json-tool
;; Package-Requires: ((emacs "26.2"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;  The Joegame Asset Tool. Various functions for joegame asset manipulation.
;;
;;; Code:

(require 'json)
(require 'cl-lib)

(require 'f)


(defvar jat-json-data nil "The json file.")

(setq jat-json-data
    (let ((json-object-type 'plist))
        (json-read-file "/home/mik/joegame/assets/data.json")))


(defun jat-image-size (PATH)
    "Gets dimensions of image found at PATH."
    (let* ((sout
               (shell-command-to-string (format  "identify -ping -format \"%%w %%h \" %s" PATH)))
              (split
                  (split-string  sout)))
        (list
            :width
            (string-to-number (car split))
            :height
            (string-to-number(nth 1 split)))))

(defun jat-image-width (PATH)
    "Width of image at PATH."
    (plist-get (jat-image-size PATH) :width))

(defun jat-image-height (PATH)
    "Height of image at PATH."
    (plist-get (jat-image-size PATH) :height))

(defun jat-get-dimension-count (ISIZE TSIZE &optional MARGIN SPACING)
    (unless MARGIN (setq MARGIN 0))
    (unless SPACING (setq SPACING 0))
    (let ((curr MARGIN)
             (count 0))
        (while (< curr ISIZE)
            (setq count (+ 1 count)
                curr (+ curr TSIZE SPACING)))
        count))


(defun jat--update-key (L KEY VAL)
    "Update the KEY of L, an alist, with VAL."
        (setf (alist-get KEY L nil 'remove) VAL))

(let ((ll '(aa (a . b) (c . d) (e . f))))
    (setf (alist-get 'a ll nil 'remove) 'r)
    ll)

(defun jat--assure-key (L KEY DVAL)
    "Assure a value to KEY is in L and fill with DVAL if not."
    (if (not (alist-get KEY (cdr L)))
        (jat--update-key L KEY DVAL)))

(defun jat--collect-all-images (L)
    "Collect all image entries from L.

    Includes entries from the older spritesheets, tilesets, images."
    (let
        ((spritesheets (assoc 'spritesheet L))
            (images (assoc 'image L)))
        (append spritesheets images)))

(defun jat-normalize-image (IMAGE)
    "For normalizing an image across older tileset/spritesheet/image definitions.
Takes an IMAGE and returns a new object."
    (let ((val (cdr IMAGE)))
        (jat--assure-key val 'frame-width 32))
    IMAGE)


(defun jat--make-keys-filenames ()
    "Replace all keys in images with their actual basename."
    (dolist (im (cdr (assoc 'image jat-data)))
        (let ((filename (f-filename (format "%s" (alist-get 'url (cdr im))))))
            (jat--update-key (cdr im) 'key filename))))


(defun jat-write-file (DATA PATH)
    "Write DATA to json PATH."
    (f-write-text (json-encode DATA) 'utf-8 PATH)
    (jat-json-format-file PATH))





(defun jat-parse-csv-file (FILE &optional SEP)
    "Parse a csv FILE into a list of lists.  The recognized seperator on lines
is SEP or a comma."
    (with-temp-buffer
        (insert-file-contents FILE)
        (let ((lines (split-string (buffer-string) "[\r\n]")))
            (cl-map 'list #'(lambda (li) (split-string li (or SEP ","))) lines))))




;; (setq jat-csv-dat
;;     (let ((out '())
;;              (raw (jat-parse-csv-file "./data.csv")))
;;         (dolist (cat raw)
;;             (if (or
;;                     (equal "spritesheet" (cl-second cat))
;;                     (equal "image" (cl-second cat)))
;;                 (setq out (append out (list cat)))))
;;         out))

(defun jat-filter-csv (CSV VAL IDX)
    (let ((out '()))
    (dolist (cat CSV)
        (if
            (equal VAL (nth IDX cat))
            (setq out (append out (list cat)))))
        out))

(defun jat-set-json (DATA KEY VAL)
    (setq DATA (plist-put DATA KEY VAL)))





;; Stolen from doom lib
(defun jat-plist-keys (PL)
    "Return keys of PL, a plist."
    (let (keys)
        (while PL
            (push (car PL) keys)
            (setq PL (cddr PL)))
        keys))

(defun jat-map-plist-keys (PLIST FUNC)
    (let* ((keys (jat-plist-keys PLIST)))
        (dolist (im keys)
            (let ((entry (plist-get PLIST im)))
                (funcall FUNC entry)))
        PLIST))

(defun jat-lookup-csv-entry (CSV IDX VAL)
    (let (out)
        (dolist (line CSV)
            (let ((tl (nth IDX line)))
                (if (equal tl VAL)
                    (setq out line))))
        out))


(defun jat-find-csv-creator (CSV VAL)
    (if-let ((iline (jat-lookup-csv-entry
                        CSV 2 VAL)))
                (if (equal (nth 1 iline) "image")
                         ;; is an image line
                         (format "%s -- %s" (nth 5 iline) (nth 6 iline))
                         ;; is a spritesheet line
                         (format "%s -- %s" (nth 10 iline) (nth 11 iline)))))


;; (jat-map-plist-keys
;;     (plist-get jat-json-data :image)
;;     #'(lambda (entry)
;;           (setq entry (plist-put entry :source (jat-find-csv-creator jat-csv-dat (plist-get entry :key))))))


;; (jat-write-file jat-json-data "data_o.json")


(defun jat-plist-access (PLIST &rest KEYLIST)
    "Access nest prop of PLIST, defined by a KEYLIST where
each successive item in the list is the next key to access"
    (if-let*   ((klist (if (listp (car KEYLIST))
                           (car KEYLIST)
                           KEYLIST))
                   (curr (plist-get PLIST (car klist)))
                   (rest (cdr klist)))
        (jat-plist-access curr rest)
        curr))

(jat-plist-access jat-json-data  :image :canyon)




(defun jat--gen-spritesheet-entry (KEY AL FC SOURCE)
    (list :key KEY :animLength AL
        :frameConfig (list :frameWidth (car FC) :frameHeight (cadr FC) :margin (caddr FC) :spacing (cadddr FC))
        :source SOURCE))

(defun jat--gen-tilemapbject-entry (NAME IMAGE TILES WIDTH)
    (list
        ;; :name NAME
        :req_image (vconcat (list IMAGE))
        :tile_config (list
                         :width (string-to-number WIDTH)
                         :texture IMAGE
                         :tiles (vconcat (cl-map 'vector #'string-to-number (s-split "," TILES))))))

(defun jat--gen-character-entry (NAME TEXT NORTH SOUTH EAST WEST)
    (list
        :name NAME
        :texture: TEXT
        :anims (list
                   :north (vconcat (cl-map 'vector #'string-to-number (s-split "," NORTH)))
                   :south (vconcat (cl-map 'vector #'string-to-number (s-split "," SOUTH)))
                   :east (vconcat (cl-map 'vector #'string-to-number (s-split "," EAST)))
                   :west (vconcat (cl-map 'vector #'string-to-number (s-split "," WEST))))))



(defun jat-batch-images (IMNAMES)
    "Add each of IMS, a list of strings, to the data."
    (let ((ims (plist-get jat-json-data :image)))
        (dolist (key IMNAMES)
            (let* ((fpath (expand-file-name (format "~/joegame/assets/images/%s" key)))
                      (imwidth (jat-image-width fpath))
                      (imheight (jat-image-height fpath)))
                (plist-put
                    ims
                    (intern(format ":%s" (f-no-ext key)))
                    (jat--gen-spritesheet-entry  (f-no-ext key) -1
                        (list  (/ imwidth 3)  (/ imheight 4)  0  0)
                        "finalbossblues -- http://www.timefantasy.net/"))))))

(defun jat-batch-chars (IMNAMES)
    "Add each of IMS, a list of strings, to the data."
    (let ((ims (plist-get jat-json-data :character)))
        (dolist (key IMNAMES)
            (let* ((text (f-no-ext key))
                      (name (substring text 3)))
                (plist-put
                    ims
                    (intern(format ":%s" name))
                    (jat--gen-character-entry name text
                        "9,10,11"
                        "0,1,2"
                        "6,7,8"
                        "3,4,5"))))))


(defun jat-add-image (NAME FILE TILES WIDTH)
    (let ((mo (plist-get jat-json-data :mapobject)))
        (plist-put mo (intern (format ":%s" NAME))
            (jat--gen-tilemapbject-entry
                NAME
                FILE
                TILES
                WIDTH))))

(defun jat-add-character (NAME TEXT NORTH SOUTH EAST WEST)
    (let ((mo (plist-get jat-json-data :character)))
        (plist-put mo (intern (format ":%s" NAME))
            (jat--gen-character-entry
                NAME
                TEXT
                NORTH
                SOUTH
                EAST
                WEST))))


(defun jat-json-format ()
    (interactive)
    (shell-command-on-region
        (point-min)
        (point-max)
        "jq"
        t
        t)
    (save-buffer))

(defun jat-json-format-file (FILE)
    (save-excursion
        (with-temp-buffer
            (find-file FILE)
            (call-interactively #'jat-json-format)
            (save-buffer)
            (kill-current-buffer))))



(defun jat-fix-anim-data ()
    (let ((chars (plist-get jat-json-data :character)))
        (dolist (charkey (jat-plist-keys chars))
            (let* ((char (plist-get chars charkey))
                      (anims (plist-get char :anims)))
                (dolist (animkey '(:north :south :east :west))
                    (plist-put anims animkey
                        (let ((num (string-to-number
                                       (caddr
                                           (string-split
                                               (plist-get anims animkey) "_")))))
                            (list num (+ 1 num) (+ 2 num)))))))))
;; (jat-fix-anim-data)
;; (jat-write-file jat-json-data "./data.json")

(defun jat-finalize-image-data (KEY)
    (let* ((itemkey (jat-plist-access jat-json-data :image KEY :key))
              (basepath "./images/")
              (url (format "%s%s.png" basepath itemkey))
              (fc (jat-plist-access jat-json-data :image KEY :frameConfig)))
        (if fc
            (let* ((isize (jat-image-size url))
                      (twidth (plist-get fc :frameWidth))
                      (theight (plist-get fc :frameHeight))
                      (margin (plist-get fc :margin))
                      (spacing (plist-get fc :spacing))
                      (iwidth (plist-get isize :width))
                      (iheight (plist-get isize :height))
                      (cols (jat-get-dimension-count iwidth twidth margin spacing))
                      (rows (jat-get-dimension-count iheight theight margin spacing)))
                (plist-put fc :imageheight iheight)
                (plist-put fc :imagewidth iwidth)
                (plist-put fc :tilecount (* rows cols))
                (plist-put fc :columns cols)))))

;; (jat-write-file jat-json-data "./data.json")


(provide 'jat)
;;; jat.el ends here
