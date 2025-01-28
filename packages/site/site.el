(require 'org)

(setq
 org-publish-use-timestamps-flag nil
 org-publish-timestamp-directory "./.org-timestamps/")

(setq
 org-publish-project-alist '(("orgfiles"
                              :base-directory "./src"
                              :recursive t
                              :publishing-directory "./dist"
                              :html-head "<link rel=\"stylesheet\" href=\"/style.css\" type=\"text/css\"/>"
                              :html-postamble nil
                              :auto-sitemap t
                              :html-postamble-format ""
                              :html-preamble-format (("en" "<header> <h2 style=\"flex-grow:1 \"><a href=\"/\">joegame</a></h2> <img class=\"gif-image\" src=\"/images/kangaroo_east.gif\"</img> <h4><a href=\"/blog\">blog</a></h4></header>"))
                              :base-extension "org"
                              :headline-levels 3
                              :section-numbers nil
                              :with-toc nil
                              :with-todo-keywords nil
                              :org-html-html5-fancy t
                              :org-html-head-include-default-style nil)

                             ("static"
                              :base-directory "./src"
                              :recursive t
                              :base-extension "jpg\\|gif\\|png\\|css\\|svg\\|webp"
                              :publishing-directory "./dist"
                              :publishing-function org-publish-attachment)

                             ("site" :components ("orgfiles" "static"))))
(org-publish "site")
