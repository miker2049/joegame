;;; joegame.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Mike Russo
;;
;; Author: Mike Russo <https://github.com/miker2049>
;; Maintainer: Mike Russo <russomichaelb@gmail.com>
;; Created: January 31, 2022
;; Modified: January 31, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://gitlab.com/joegame/joegame
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; The joegame elisp toolkit, makeshift toolkit. To help generate joegame files, and other stuff
;;
;;
;;; Code:

;; (require 'widget)
;; (require 'emacsql-sqlite)
(defun joegame-hello (NAME)
    "Say hello to you, NAME."
    (interactive "MName:  " )
    (message "Helloooo %s" NAME))



;; (eval-when-compile
;;   (require 'wid-edit))


;; (defvar widget-example-repeat)

;; (defun joegame-widget-example ()
;;     (defvar joegame-db (emacsql-sqlite (expand-file-name "~/projects/joegame/assets/jdb.db")))
;;     "Create the widgets from the Widget manual."
;;     (interactive)
;;     (switch-to-buffer "*Widget Example*")
;;     (kill-all-local-variables)
;;     (make-local-variable 'widget-example-repeat)
;;     (let ((inhibit-read-only t))
;;         (erase-buffer))
;;     (remove-overlays)
;;     (widget-insert "Here is some documentation.\n\n")
;;     (widget-create 'editable-field
;;         :size 13
;;         :format "Name: %v " ; Text after the field!
;;         "My Name")
;;     (widget-create 'menu-choice
;;         :tag "Choose"
;;         :value "This"
;;         :help-echo "Choose me, please!"
;;         :notify (lambda (widget &rest ignore)
;;                     (message "%s is a good choice!"
;;                         (widget-value widget)))
;;         '(item :tag "This option" :value "This")
;;         '(choice-item "That option")
;;         '(editable-field :menu-tag "No option" "Thus option"))
;;     (widget-create 'editable-field
;;         :format "Address: %v"
;;         "Some Place\nIn some City\nSome country.")
;;     (widget-insert "\nSee also ")
;;     (widget-create 'link
;;         :notify (lambda (&rest ignore)
;;                     (widget-value-set widget-example-repeat
;;                         '("En" "To" "Tre" "hooha" "hippie"))
;;                     (widget-setup))
;;         "other work")
;;     (widget-insert
;;         " for more information.\n\nNumbers: count to three below\n")
;;     (setq widget-example-repeat
;;         (widget-create 'editable-list
;;             :entry-format "%i %d %v"
;;             :notify
;;             (lambda (widget &rest ignore)
;;                 (let ((old (widget-get widget
;;                                ':example-length))
;;                          (new (length (widget-value widget))))
;;                     (unless (eq old new)
;;                         (widget-put widget ':example-length new)
;;                         (message "You can count to %d." new))))
;;             :value (mapcar (lambda (i) (format! "%s" i)) (emacsql joegame-db [:select [id creator_name]
;;                                                                         :from creators]))
;;             '(editable-field :value "three")))
;;     (widget-insert "\n\nSelect multiple:\n\n")
;;     (widget-create 'checkbox t)
;;     (widget-insert " This\n")
;;     (widget-create 'checkbox nil)
;;     (widget-insert " That\n")
;;     (widget-create 'checkbox
;;         :notify (lambda (&rest ignore) (message "Tickle"))
;;         t)
;;     (widget-insert " Thus\n\nSelect one:\n\n")
;;     (widget-create 'radio-button-choice
;;         :value "One"
;;         :notify (lambda (widget &rest ignore)
;;                     (message "You selected %s"
;;                         (widget-value widget)))
;;         '(item "One") '(item "Another One.")
;;         '(item "A Final One."))
;;     (widget-insert "\n")
;;     (widget-create 'push-button
;;         :notify (lambda (&rest ignore)
;;                     (if (= (length
;;                                (widget-value widget-example-repeat))
;;                             3)
;;                         (message "Congratulation!")
;;                         (error "Three was the count!")))
;;         "Apply Form")
;;     (widget-insert " ")
;;     (widget-create 'push-button
;;         :notify (lambda (&rest ignore)
;;                     (joegame-widget-example))
;;         "Reset Form")
;;     (widget-insert "\n")
;;     (use-local-map widget-keymap)
;;     (widget-setup))

(defun joegame-edit-markdown-comment ()
    "Edit a md comment in another window."
    (interactive)
    (let
        ;; ((edit-indirect-after-creation-hook '())
        ;;      (edit-indirect-before-commit-hook '())
        ;;      (edit-indirect-after-commit-functions '()))
        ((edit-indirect-after-creation-hook '((lambda ()
                                                   (goto-char (point-min))
                                                   (while (not (eobp))
                                                       (back-to-indentation)
                                                       (delete-char 1)
                                                       (forward-line 1))
                                                   (point-min)
                                                   (markdown-mode)))))

        ;; (edit-indirect-before-commit-hook '((lambda ()
        ;;                                                  ;; (goto-char (point-min))
        ;;                                                  ;; (while (not (eobp))
        ;;                                                  ;;     (back-to-indentation)
        ;;                                                  ;;     (insert-char #x2A)
        ;;                                                  ;;     (insert-char #x20)
        ;;                                                  ;;     (forward-line 1))
        ;;                                         ))))
        (er/mark-comment)
        (forward-line)
        (exchange-point-and-mark)
        (forward-line -1)
        (end-of-line)
        (switch-to-buffer-other-window
                (edit-indirect-region (region-beginning) (region-end)))))



(provide 'joegame)
;;; joegame.el ends here
