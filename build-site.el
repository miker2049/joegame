#!/usr/bin/env -S nix run .#emacss -- --script

(require 'org)
(require 'ox)
(require 'ob)
(require 'ob-emacs-lisp)
(require 'ob-shell)
(require 'ob-python)
;; (require 'ob-tangle)
;; (require 'htmlize)

(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
         (shell . t)
         (python . t)
         (lilypond . t)
         (org . t)))
(let* ((org-export-htmlize-output-type nil)
              (src (expand-file-name "~/joegame/site"))
              (dest (expand-file-name "~/joegame/public")))
        (setq org-publish-project-alist `(("org"
                                              :publishing-directory ,dest
                                              :base-directory ,src
                                              :with-toc nil
                                              :html-head-include-default-style nil
                                              :html-doctype "html5"
                                              :htmlized-source nil
                                              :with-title 't
                                              :recursive nil
                                              :with-broken-links 't
                                              :html-html5-fancy 't)
                                             ("static"
                                                 :base-directory ,src
                                                 :base-extension "css\\|svg\\|html\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|ttf\\|ico"
                                                 :publishing-directory ,dest
                                                 :recursive 't
                                                 :publishing-function org-publish-attachment)
                                             ;; ("dld-scss"
                                             ;;     :base-directory ,src
                                             ;;     :base-extension "scss"
                                             ;;     :publishing-directory ,dest
                                             ;;     :recursive t
                                             ;;     :publishing-function dld-org-publish-scss)
                                             ("main" :components ("org" "static")))))
(defun build-joegame-site ()
    (interactive)
    (org-publish "main" 't))


(defun export-it (IN OUT)
    (with-temp-buffer
        (insert-file-contents (expand-file-name IN))
        (org-export-to-file 'html (expand-file-name OUT)
            nil nil nil nil
            '(:publishing-directory ,dest
                 :base-directory ,src
                 :with-toc nil
                 :html-head-include-default-style nil
                 :html-doctype "html5"
                 :htmlized-source nil
                 :with-title 't
                 :recursive nil
                 :with-broken-links 't
                 :html-html5-fancy 't))))



(if-let ((infile  (car  command-line-args-left))
             (outfile (cadr command-line-args-left)))
    (export-it infile outfile)
    (print "need two args <infile> <outfile>"))
