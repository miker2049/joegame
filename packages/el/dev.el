;;; dev --- joegame dev elisp tools
;;; dev.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Mike Russo
;;
;; Author: Mike Russo <russomichaelb@gmail.com>
;; Maintainer: Mike Russo <russomichaelb@gmail.com>
;; Created: June 29, 2022
;; Modified: June 29, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/miker2049/dld
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Functions and variables to build the joegame dev-log.
;;
;;; Code:

(require 'org)
(require 'filenotify)


(defvar dev-server-process-buffer-name "*JOEGAME HTTP SERVER*")
(defvar dev-server-process-name "joegame-http-server")
(defvar dev-server-process nil)
(defvar dev-joegame-directory (expand-file-name "~/joegame") "The base directory where there is joegame.")
(defvar dev-server-directory  (concat dev-joegame-directory "/public"))
(defvar dev-filewatcher nil)
(defvar dev-server-http-server-port 42069)

(defvar dev-make-path (replace-regexp-in-string "\n$" ""  (shell-command-to-string "which make")))




(defun dev--try-delete-server ()
    (if (processp dev-server-process)
        (delete-process dev-server-process)))

(defun dev-start-dev-server ()
  "Start or restart the dev server for joegame."
    (progn (dev--try-delete-server)
        (setq dev-server-process (start-process dev-server-process-name
                                         dev-server-process-buffer-name
                                         "python"
                                         "-m"
                                         "http.server"
                                         "-d"
                                         dev-server-directory
                                         (format "%s" dev-server-http-server-port)))))

(defun dev-save-publish-restart-server ()
    (interactive)
    (progn
        (save-buffer)
        (with-temp-buffer
            (call-process dev-make-path nil "*JOEGAME MAKE*" nil "-C" dev-joegame-directory "site"))
        (dev-start-dev-server)))

(defun dev--filewatch-callback (ev)
    (if (eq (cadr ev) 'changed)
        (dev-save-publish-restart-server)))

(defun dev-watch-dev ()
    (interactive)
    (progn
        (dev-start-dev-server)
        (if (file-notify-valid-p dev-filewatcher)
            (message "hallow")
            (file-notify-rm-watch dev-filewatcher))
        (setq dev-filewatcher
            (file-notify-add-watch
                (concat dev-joegame-directory "/site")
                '(change)
                #'dev--filewatch-callback))))

(defun dev--try-rm-watch ()
    (interactive)
    (if (file-notify-valid-p dev-filewatcher)(file-notify-rm-watch dev-filewatcher)))

(defun dev-down ()
    (interactive)
    (progn
        (dev--try-rm-watch)
        (dev--try-delete-server)))
;; (dev-watch-dev)

(dolist (d (hash-table-keys file-notify-descriptors))
    (file-notify-rm-watch d))

(provide 'dev)
;;; dev.el ends here
