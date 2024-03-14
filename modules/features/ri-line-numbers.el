;;; ri-line-numbers --- provides line numbers -*- lexical-binding: t; -*-

;;; Commentary:
;; Note: Enabling line numbers will come with quirks.  Line numbers
;; are sometimes not appropriate for certain modes, and they will
;; manually need to be excluded from having line numbers.

;;; Code:

;;; --- Line numbers: ----

(global-display-line-numbers-mode t)  ;; add line numbers
(column-number-mode 1) ; (columns on modeline)

;; line number mode exceptions
;; TODO: rewrite all this into disabling mode per pkg hook.
(dolist (mode '(org-mode-hook
                dired-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                eshell-mode-hook
                image-minor-mode-hook
                doc-view-mode-hook
                treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


(provide 'ri-line-numbers)
;;; ri-line-numbers.el ends here
