(require 'org)

(setq
 org-publish-use-timestamps-flag nil
 org-publish-timestamp-directory "./.org-timestamps/")
(setq
 org-publish-project-alist '(("orgfiles"
                              :base-directory "./src"
                              :publishing-directory "./dist"
                              :html-head "<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\"/>"
                              :base-extension "org"
                              :headline-levels 3
                              :section-numbers nil
                              :with-toc nil
                              :with-todo-keywords nil
                              :org-html-html5-fancy t
                              :org-html-head-include-default-style nil)
                             ("static"
                              :base-directory "./public"
                              :base-extension "jpg\\|gif\\|png\\|css\\|svg\\|webp"
                              :publishing-directory "./dist"
                              :publishing-function org-publish-attachment)
                             ("site" :components ("orgfiles" "static"))))
(org-publish "site")
