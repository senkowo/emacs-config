;;; ri-lang-scheme --- scheme lang setup -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'setup)

(setup scheme-mode
  (:file-match "\\.sld\\'")
  (:hook guix-devel-mode flycheck-mode)) ; in scheme-mode-hook, add these.

(setup (:pkg geiser)
  (setq geiser-default-implementation 'guile)
  (setq geiser-active-implementations '(guile))
  (setq geiser-implementations-alist '(((regexp "\\.scm$") guile))))

(setup (:pkg geiser-guile))

(provide 'ri-lang-scheme)
;;; ri-lang-scheme.el ends here
