;;; jat-interface.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Mike Russo
;;
;; Author: Mike Russo <russomichaelb@gmail.com>
;; Maintainer: Mike Russo <russomichaelb@gmail.com>
;; Created: February 06, 2023
;; Modified: February 06, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/mik/jat-interface
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'jat)

(defun jat-interface-objects-func ()
    (cl-map 'list
        #'(lambda (item)
              (let* ((entry (jat-plist-access jat-json-data (list :image item )))
                        (key (jat-plist-access entry :key))
                        (source (jat-plist-access entry :source))
                        (fw (jat-plist-access entry :frameConfig :frameWidth))
                        (fh (jat-plist-access entry :frameConfig :frameHeight)))
                  (print entry)
                  (list key (vector key
                                (or source "unknown")
                                (number-to-string (or fw -1))
                                (number-to-string (or fh -1))))))
        (jat-plist-keys (plist-get jat-json-data :image))))


(equal (vector 1 2 3) [1 2 3 4])
(jat-interface-objects-func)


(number-to-string nil)


(defun jat-list-mapobjects ()
    (interactive)
    (let ((buff (get-buffer-create "*JATI-O*")))
        (switch-to-buffer buff)
        (jat-interface-objects)
        (setq-local
            tabulated-list-entries
            #'jat-interface-objects-func)
        (tabulated-list-print)))



(define-derived-mode jat-interface-objects tabulated-list-mode "jat-O"
    "See all mapobjects as a list."
    (setq tabulated-list-format
        [("key" 32 't)
                     ("source" 44 't)
                     ("fw" 24 nil)
                     ("fh" 24 nil)]
        tabulated-list-printer
        #'tabulated-list-print-entry)
    (tabulated-list-init-header))

(provide 'jat-interface)
;;; jat-interface.el ends here
