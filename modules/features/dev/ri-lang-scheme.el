;;; ri-lang-scheme --- scheme lang setup -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'setup)

(setup scheme-mode
  (:file-match "\\.sld\\'")
  (:hook guix-devel-mode))

(setup (:pkg geiser)
  (setq geiser-default-implementation 'guile)
  (setq geiser-active-implementations '(guile))
  (setq geiser-implementations-alist '(((regexp "\\.scm$") guile))))

(setup (:pkg geiser-guile))

;; Enable flycheck
(add-hook 'scheme-mode-hook #'flycheck-mode)

;;; ri-lang-scheme.el ends here
