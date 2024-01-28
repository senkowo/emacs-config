;;; ri-swap-x-and-u --- swap some keybinds to better accomidate dvp keybinds -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; org binding on M-t so make all t key bindings translate to p ?

;; swap ctrl and alt keys, since it's easier to press ctrl with the thumb

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (global-set-key (kbd "C-t") 'previous-line)
	    ;; make C-h 'prev-line instead? (make sure to git commit pull and push before, and
	    ;;  don't push for a while. So to revert, simply revert to remote origin main head).
	    ;;  (maybe create a variable and if true change certain keybinds for modes?
	    ;; One issue with swapping is that C-f C-b C-h C-n becomes harder to do and possibly,
	    ;;  /possibly/ harder to do.

	    (global-set-key (kbd "C-u") ctl-x-map)
	    ;; (global-set-key (kbd "C-z") 'universal-argument)
	    ;; (global-set-key (kbd "C-z") help-map)
	    (global-set-key (kbd "M--") 'universal-argument) ; TODO: move this elsewhere
	    ;; (global-set-key (kbd "C-M-g") 'universal-argument)

	    ;; what if i bind "C-c z" to help-map so that i can do "SPC z" in
	    ;; meow-leader-mode-map to access "C-z" options? Should work...

	    ))


(provide 'ri-swap-x-and-u)
;;; ri-swap-x-and-u.el ends here
