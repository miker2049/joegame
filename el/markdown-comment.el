;;; markdown-comment.el --- Open comments as markdown buffer -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Mike Russo
;;
;; Author: Mike Russo <russomichaelb@gmail.com>
;; Maintainer: Mike Russo <russomichaelb@gmail.com>
;; Created: July 13, 2022
;; Modified: July 13, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/miker2049/markdown-comment
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Will use built in comment functions to
;;  find an enclosing comment, and if so open it as
;;  a markdown-mode buffer.
;;
;;; Code:

(defun markdown-comment-js ()
  "Open enclosing comment as md-buffer."
  (interactive)
  (comment-beginning)
  (move-beginning-of-line nil)
  (set-mark-command nil)
  (search-forward "*/")
  (setq deactivate-mark nil))

(provide 'markdown-comment)
;;; markdown-comment.el ends here
