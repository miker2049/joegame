
(cd (expand-file-name "~/joegame"))

;; (setq package-user-dir (concat default-directory ".script-elpa"))
;; (package-install 'f)
;; (package-activate-all)

(print (format "%s" command-line-args))


;; (load-file "assets/jat.el")
(require 'jat (concat default-directory "assets/jat.el"))

(cl-destructuring-bind (name file north south east west)
    command-line-args-left
    (jat-add-character name file north south east west))

(jat-write-file jat-json-data "/home/mik/joegame/assets/data.json")
