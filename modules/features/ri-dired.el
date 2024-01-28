;;; ri-dired --- configurations for dired -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;
;;
;;
;;
;;
;;

;;; Code:

(require 'setup)

;;; --- Dired: ----

(setup dired
  (setq dired-listing-switches "-agho --group-directories-first"
	dired-dwim-target t ; auto select dir to move to if another dired window open.
	delete-by-moving-to-trash t)
  ;; TODO: add stuff for evil mode here
  )

;; reuse dired buffers
;; TODO: implement reusing dired buffers
;; (setup dired-single)

;;; --- misc: ----

;; Icons to dired
(setup (:pkg all-the-icons-dired)
  ;; TODO: must disable hook if dirvish-override-dired-mode eq nil
  (:when-loaded
    (add-hook 'dired-mode-hook
	      (lambda ()
		(all-the-icons-dired-mode dirvish-override-dired-mode)))))

;; Open certain files using external programs
(setup (:pkg dired-open)
  (setq dired-open-extensions
        '(("mkv" . "mpv")
          ;; ("png" . "feh")
          ("docx" . "libreoffice"))))

;; TODO: add dired-hide-dotfiles here


;;; --- leader key defs: ----

(leader-key-def
  "d"  '(:ignore t :which-key "dired")
  "dd" 'dired
  "di" 'dired-jump
  ;; "dh" 'ri/dired-hide-dotfiles-mode-toggle ; TODO!
  )

(provide 'ri-dired)
;;; ri-dired.el ends here
