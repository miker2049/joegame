;;; Package -- joegame
;;; Commentary:
;;;
;;; A collection of utilities for joegame developemt and jdb management in Emacs.
;;; Code:
(require 'f)
(require 'org)
(require 'org-element)
(require 'ox)
(require 'project)

(require 'sqlite3)
(use-package! sqlite3)

(defvar joegame/dev-server-process-buffer-name "*JOEGAME HTTP SERVER*")
(defvar joegame/dev-server-process-name "joegame-http-server")
(defvar joegame/joegame-directory (expand-file-name "~/projects/joegame") "The base directory where there is joegame.")
(defvar joegame/dev-server-directory  (concat joegame/joegame-directory "/public"))
(defvar joegame/jdb-path  (concat joegame/joegame-directory "/assets/jdb.db")
    "The directory where the dev server points.")

(defvar joegame/dev-server-http-server-bin
    (let ((local-bin (concat joegame/joegame-directory
                         "/node_modules/http-server/bin/http-server")))
        (if (f-executable? local-bin)
            local-bin
            "http-server"))
    "The executable availble to make an http server with supplied directory.")

(setq org-publish-project-alist '())
(let ((basedir (expand-file-name (project-root (project-current))))
         (org-export-htmlize-output-type nil)
         (dld-copy-css (lambda (_PROP) (shell-command
                                           (format "%scopy-tufte.sh" (expand-file-name (project-root (project-current))))))))
    (add-to-list 'org-publish-project-alist `("dld"
                                                 :publishing-directory
                                                 ,(concat basedir "public/dld")
                                                 :base-directory
                                                 ,(concat basedir "site/dld")
                                                 :with-toc nil
                                                 :html-head-include-default-style nil
                                                 :html-doctype "html5"
                                                 :htmlized-source nil
                                                 :htmlize-output nil
                                                 :with-title 't
                                                 :recursive 't
                                                 :with-broken-links 't
                                                 :html-html5-fancy 't
                                                 :completion-function dld-copy-css
                                                 ;; :preparation-function prep-fun
                                                 :html-head "<link rel=\"stylesheet\" href=\"../tufte.min.css\" type=\"text/css\"/>")))

;; (setq org-attach-dir-relative 't)
(defun joegame/start-dev-server ()
    "Start or restart the dev server for joegame."
    (delete-process joegame/dev-server-process-buffer-name)
    (start-process joegame/dev-server-process-name
        joegame/dev-server-process-buffer-name
        joegame/dev-server-http-server-bin
        joegame/dev-server-directory))

(defun joegame/save-publish-restart-server ()
    (interactive)
    (save-buffer)
    (org-publish "dld")
    (joegame/start-dev-server))

(defun joegame/org-string-to-inner-html (INPUT)
    "Returns a HTML string from INPUT, without containing tag.  For shortcodes."
    (let* ((peed (replace-regexp-in-string "\n" ""  (org-export-string-as INPUT 'html t)))
              (unpeed (replace-regexp-in-string "</?p>" "" peed)))
        unpeed))

(defun joegame/tufte-sidenote (INPUT)
    "Tufte style sidenote"
    (let ((hash-id (substring (sha1 INPUT) 0 8))
             (finput (joegame/org-string-to-inner-html INPUT)))
        (format "<label for=\"%s\" class=\"margin-toggle sidenote-number\"></label>
  <input type=\"checkbox\" id=\"%s\" class=\"margin-toggle\"/>
  <span class=\"sidenote\"> %s </span>" hash-id hash-id finput)))


(defun joegame/org-string-to-inner-html (INPUT)
    "Return a HTML string from INPUT, without containing tag.  For shortcodes."
    (let* ((peed (replace-regexp-in-string "\n" ""  (org-export-string-as INPUT 'html t)))
              (unpeed (replace-regexp-in-string "</?p>" "" peed)))
        unpeed))

(defun joegame/tufte-marginnote (INPUT)
    "Tufte style sidenote"
    (let ((hash-id (substring (sha1 INPUT) 0 8))
             (finput (joegame/org-string-to-inner-html INPUT)))
        (format "<label for=\"%s\" class=\"margin-toggle\">&#8853;</label>
               <input type=\"checkbox\" id=\"%s\" class=\"margin-toggle\"/>
               <span class=\"marginnote\"> %s </span>" hash-id hash-id finput)))

(defun joegame/tufte-imagee (IMG &optional CAPTION)
    "Tufte styled img within a figure with caption."
    (let ((fimg (joegame/org-string-to-inner-html IMG))
             (fcap (joegame/org-string-to-inner-html (or CAPTION ""))))
        (format "<figure> %s <figcaption> %s </figcaption> </figure>" fimg fcap)))

(defun tufte-org-html-footnote-reference (ref contents info)
    (let* ((definition (org-export-get-footnote-definition ref info))
              (export (org-export-data-with-backend definition 'org info))
              (label (org-element-property :label ref)))
        (if (eq (string-to-number label) 0)
            (joegame/tufte-marginnote export)
            (joegame/tufte-sidenote export))))

(advice-add 'org-html-footnote-reference :override #'tufte-org-html-footnote-reference)
(advice-add 'org-html-footnote-section :override (lambda (_X) ""))

(defvar joegame/jdb (sqlite3-open joegame/jdb-path sqlite-open-readwrite)
    "The default sqlite api object")

(defun joegame/jdb-file-hash (&optional file-path)
    "Compute the hash of FILE-PATH, a file or current buffer. Stolen from org roam kinda :)."
    (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally file-path)
        (secure-hash 'sha256 (current-buffer))))

(defun joegame/jdb-insert-asset-url (NAME URL CREATOR)
    "Insert asset named NAME and by CREATOR from a URL."
    (interactive "sName:\nsUrl:\nsCreator/Author:")
    (let* ((tmpf (make-temp-name "jdb-asset-insert")))
        (url-copy-file URL tmpf)
        (joegame/jdb-insert-asset NAME tmpf URL CREATOR)))

(defun joegame/jdb-insert-image (NAME FILE SOURCE CREATOR FRAMEW FRAMEH MARGIN SPACING)

    "Insert an asset into jdb. Giving NAME, FILE path, SOURCE, CREATOR, FRAMEW and FRAMEH, MARGIN, SPACING. The frame dimensions can
    be -1 if it is just an image and not spritesheet/tilesheet Doing this with a \"call-process\" because the other module doesn't
    support blobs."

    (call-process "sqlite3" nil  "joegamesqlitee" nil joegame/jdb-path
        (format "INSERT INTO images(name, data, frame_width, frame_height, margin, spacing, creator, source, hash) VALUES (\"%s\",
 readfile(\"%s\"), \"%s\", \"%s\", \"%s\", \"%s\", \"%s\", \"%s\", \"%s\")"
            NAME
            (expand-file-name FILE)
            FRAMEW
            FRAMEH
            MARGIN
            SPACING
            CREATOR
            SOURCE
            (joegame/jdb-file-hash (expand-file-name FILE))))
    (message "hoopah"))

(defun joegame/jdb-insert-image-interactive (NAME FILE SOURCE CREATOR FRAMEW FRAMEH MARGIN SPACING)
    (interactive "sName:\nfFile:\nsSource:\nsCreator:\nsFrame width\nsFrame height:\nsMargin\nsSpacing:")
    (joegame/jdb-insert-image NAME FILE SOURCE CREATOR FRAMEW FRAMEH MARGIN SPACING))


(defun joegame/jdb-insert-creator (NAME URL)
    "Inserts a creator"
    (interactive "sName:\nsURL:")
    (require 'sqlite3)
    (let* ((db (sqlite3-open joegame/jdb-path sqlite-open-readwrite sqlite-open-create))
              (stmt  (sqlite3-prepare db "insert into creators(creator_name, creator_url) values (?,?)"))
              (iname NAME) (iurl URL))
        (sqlite3-bind-multi stmt iname iurl)
        (message iname)
        (sqlite3-step stmt)
        (sqlite3-finalize stmt)
        (sqlite3-close db)))

(defmacro jdb-exec (exec &rest args)
    `(call-process "sqlite3" nil
         ,(buffer-name) nil
         ,joegame/jdb-path
         ,exec ,@args))

(defmacro sql (exec &rest params)
    `(with-temp-buffer
         (call-process "sqlite3" nil
             (buffer-name) nil
             ,joegame/jdb-path
             (format ,exec ,@params))
         (--filter (lambda (F) (length< F 1))
             (mapcar (lambda (S) (s-split "|" S))
                 (s-split "\n" (buffer-string))))))


(provide 'joegame)
;;; joegame.el ends here.
