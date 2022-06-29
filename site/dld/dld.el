;;; dld --- Build the dev log dungeon.
;;; dld.el -*- lexical-binding: t; -*-
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

(require 'ox)
(require 'ox-publish)
(require 'project)
(require 'org)
(require 'filenotify)

(defvar dld-dev-server-process-buffer-name "*JOEGAME HTTP SERVER*")
(defvar dld-dev-server-process-name "joegame-http-server")
(defvar dld-dev-server-process nil)
(defvar dld-joegame-directory (expand-file-name "~/projects/joegame/site/dld") "The base directory where there is joegame.")
(defvar dld-dev-server-directory  (concat dld-joegame-directory "/public"))
(defvar dld-filewatcher nil)
(defvar dld-dev-server-pnpm-bin "/home/mik/.local/share/pnpm/pnpm")
(defvar dld-dev-server-http-server-command "http-server"
  "The executable availble to make an http server with supplied directory.")
(defun dld--try-delete-server ()
    (if (processp dld-dev-server-process)
        (delete-process dld-dev-server-process)))

(defun dld-start-dev-server ()
  "Start or restart the dev server for joegame."
  (progn (dld--try-delete-server)
    (setq dld-dev-server-process (start-process dld-dev-server-process-name
                                                dld-dev-server-process-buffer-name
                                                dld-dev-server-pnpm-bin
                                     dld-dev-server-http-server-command
                                                dld-dev-server-directory))))

(dld-start-dev-server)

(defun dld-org-publish-scss (_plist filename pub-dir)
  "Publish a file with no transformation of any kind.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (unless (file-directory-p pub-dir)
    (make-directory pub-dir t))
  (let ((output (expand-file-name
                 (concat
                  (file-name-base (file-name-nondirectory filename))
                  ".css")
                 pub-dir)))
    (shell-command (format "pnpm sass %s %s" filename output)
                   ;; Return file name.
                   output)))

(let* ((basedir dld-joegame-directory)
       (org-export-htmlize-output-type nil)
       (src (concat basedir "/org"))
       (dest (concat basedir "/public")))
  (setq org-publish-project-alist `(("dld"
                                     :publishing-directory ,dest
                                     :base-directory ,src
                                     :with-toc nil
                                     :html-head-include-default-style nil
                                     :html-doctype "html5"
                                     :htmlized-source nil
                                     :with-title 't
                                     :recursive 't
                                     :with-broken-links 't
                                     :html-html5-fancy 't)
                                    ("dld-static"
                                     :base-directory ,src
                                     :base-extension "css\\|svg\\|html\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
                                     :publishing-directory ,dest
                                     :recursive t
                                     :publishing-function org-publish-attachment)
                                    ("dld-scss"
                                     :base-directory ,src
                                     :base-extension "scss"
                                     :publishing-directory ,dest
                                     :recursive t
                                     :publishing-function dld-org-publish-scss)
                                    ("main" :components ("dld" "dld-scss" "dld-static")))))

(defun dld-save-publish-restart-server ()
  (interactive)
  (progn (save-buffer)
         (org-publish "main")
         (dld-start-dev-server)))


(defun dld--filewatch-callback (ev)
    (if (eq (cadr ev) 'changed)
        (dld-save-publish-restart-server)))

(defun dld-watch-dev ()
  (interactive)
  (progn
    (dld-start-dev-server)
    (if (file-notify-valid-p dld-filewatcher)
        (message "hallow")
        (file-notify-rm-watch dld-filewatcher))
    (setq dld-filewatcher
          (file-notify-add-watch
              (concat dld-joegame-directory "/org")
              '(change)
              #'dld--filewatch-callback))))

(defun dld--try-rm-watch ()
  (interactive)
    (if (file-notify-valid-p dld-filewatcher)(file-notify-rm-watch dld-filewatcher)))

(defun dld-dev-down ()
    (interactive)
    (progn
        (dld--try-rm-watch)
        (dld--try-delete-server)))
(dld-watch-dev)
(provide 'dld)
;;; dld.el ends here
