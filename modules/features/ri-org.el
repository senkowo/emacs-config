;;; ri-org --- configure org-mode -*- lexical-binding: t; mode: emacs-lisp; mode: outline-minor; -*-

;;; Commentary:

;;; Code:

;;; --- Org fonts: ----

(require 'org)
(defun ri/org-font-setup ()
  (interactive)
  (set-face-attribute 'org-document-title nil :weight 'bold :height 1.5)
  (dolist (face '((org-level-1 . 1.3)
                  (org-level-2 . 1.25)
                  (org-level-3 . 1.2)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    ;; font for bullets
    (set-face-attribute (car face) nil :font "Liberation Mono" :weight 'bold :height (cdr face))
    ;; (set-face-attribute (car face) nil :font "" :weight 'bold :height (cdr face))
    )

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch :height 1.5)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)

  (set-face-attribute 'org-hide nil :inherit 'fixed-pitch) ; fixes indentation

  ;; more options that can be set:
  (set-face-attribute 'org-tag nil :inherit '(shadow) :weight 'bold)
  ;; (set-face-attribute 'org-document-info :foreground "dark orange")
  ;; (set-face-attribute 'org-document-info-keyword :inherit (shadow fixed-pitch))
  ;; (set-face-attribute 'org-indent :inherit '(org-hide fixed-pitch))
  ;; (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch)) ; hide stars
  ;; (set-face-attribute 'org-link :foreground "royal blue" :underline t)
  ;; (set-face-attribute 'org-property-value :inherit fixed-pitch)
  )


(provide 'ri-org)
;;; ri-org.el ends here
