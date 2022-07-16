;;; pnpm-workspace.el --- Tools for developing joegame in emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Mike Russo
;;
;; Author: Mike Russo <russomichaelb@gmail.com>
;; Maintainer: Mike Russo <russomichaelb@gmail.com>
;; Created: June 30, 2022
;; Modified: June 30, 2022
;; Version: 0.0.1
;; Keywords: convenience data docs help hypermedia joegame
;; Homepage: https://gitlab.com/joegame/joegame
;; Package-Requires: ((emacs "26.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 'json)
(require 'json-mode)
(require 'format-all)

(defvar pnpm-workspace-directory nil
  "The root of the pnpm monorepo project.")

(defvar pnpm-workspace-pnpm-bin  (expand-file-name "~/.local/share/pnpm/pnpm")
  "The root of the pnpm monorepo project.")


(defun pnpm-workspace--workspace-dirs (PATH)
  "Internal command to produce a list of pnpm workspace directory paths.
Look for project in PATH and up."
  (cl-remove-if
   (lambda (path)  (not  (file-exists-p
                          (concat path "/package.json"))))
   (split-string
    (shell-command-to-string
     (concat pnpm-workspace-pnpm-bin " m exec pwd"))
    "[\r\n]")))

(defun pnpm-workspace--workspace-package-parse-name (PATH)
  "Parse name from a package.json file, given a PATH."
  (shell-command-to-string (concat "cat " PATH " | jq .name")))

(defun pnpm-workspace--workspace-package-parse-version (PATH)
  "Parse name from a package.json file, given a PATH."
  (shell-command-to-string (concat "cat " PATH " | jq .version")))


(defun pnpm-workspace-go (PATH)
  "Move buffer to the base of a workspace, based on pnpm workspace.
Defined PATH gives directory where this starts looking for a pnpm project."
  (interactive (list
                (completing-read "Select workspace:"
                                 (pnpm-workspace--workspace-dirs
                                  "~/projects/joegame"))))
  (find-file PATH))

(defun pnpm-workspace--parse-json (PATH)
  "Return an elisp object representing the JSON file at PATH."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string))
    (json-read-file PATH)))

(defun pnpm-workspace--write-json (PATH OBJ)
  "Write JSON data contained in OBJ to file at PATH."
  (with-temp-file PATH
    (insert (json-encode OBJ))
    (json-mode)
    (format-all-buffer)))

(defun pnpm-workspace--all-workspace-package-config-paths (PATH)
  "Return a list of package.json files in all workspaces.
The pnpm project to search in is within PATH."
  (mapcar (lambda (item)
            (concat item "/package.json"))
          (pnpm-workspace--workspace-dirs PATH)))

(defun pnpm-workspace--change-version (PACK VER)
  "Change version of a package or app.
PACK is the path to package.json, and VER is the wanted version."
  (let ((obj (pnpm-workspace--parse-json PACK)))
    (puthash "version" VER obj)
    (pnpm-workspace--write-json PACK obj)))


(provide 'pnpm-workspace)
;;; pnpm-workspace.el ends here
