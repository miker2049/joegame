(require 'org)
(require 'ox-publish)
(let* ((org-export-htmlize-output-type nil)
          ;; (src ".")
          ;; (dest "./public")
          )
    (setq org-publish-project-alist `(("landing"
                                          :publishing-directory ,(expand-file-name "./public")
                                          :base-directory ,(expand-file-name "./site")
                                          :with-toc nil
                                          :html-head-include-default-style nil
                                          :html-doctype "html5"
                                          :htmlized-source nil
                                          :with-title 't
                                          :recursive nil
                                          ;; :exclude ".*\\.org"
                                          ;; :include "README.org"
                                          :with-broken-links 't
                                          :html-html5-fancy 't)
                                         ;; ("dld-static"
                                         ;;     :base-directory ,src
                                         ;;     :base-extension "css\\|svg\\|html\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
                                         ;;     :publishing-directory ,dest
                                         ;;     :recursive t
                                         ;;     :publishing-function org-publish-attachment)
                                         ;; ("dld-scss"
                                         ;;     :base-directory ,src
                                         ;;     :base-extension "scss"
                                         ;;     :publishing-directory ,dest
                                         ;;     :recursive t
                                         ;;     :publishing-function dld-org-publish-scss)
                                         ("main" :components ("landing"))))
    (org-publish "main"))
