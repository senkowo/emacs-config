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
  (:bind "h" dired-up-directory) ; TODO: only applicable to dvp
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

;; custom function for sorting
(defun ri/dired-sort ()
  "Sort dired dir listing in different ways.
Prompt for a choice.
URL `http://xahlee.info/emacs/emacs/dired_sort.html'
Version: 2018-12-23 2022-04-07"
  (interactive)
  (if (equal major-mode 'dired-mode)
      (let (xsortBy xarg)
	(setq xsortBy (completing-read "Sort by:" '( "date" "size" "name" )))
	(cond
	 ((equal xsortBy "name") (setq xarg "-Al "))
	 ((equal xsortBy "date") (setq xarg "-Al -t"))
	 ((equal xsortBy "size") (setq xarg "-Al -S"))
	 ((equal xsortBy "dir") (setq xarg "-Al --group-directories-first"))
	 (t (error "logic error 09535" )))
	(dired-sort-other xarg ))
    (message "Not in dired buffer.")))

;;; --- leader key defs: ----

(leader-key-def
  "d"  '(:ignore t :which-key "dired")
  "dd" 'dired
  "di" 'dired-jump
  ;; "dh" 'ri/dired-hide-dotfiles-mode-toggle ; TODO!
  "ds" 'ri/dired-sort
  )


(provide 'ri-dired)
;;; ri-dired.el ends here
