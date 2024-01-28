;;; ri-dev-gen --- general config for editing code -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'setup)

;; NOTE: this will show a bunch of error messages when editing lisp code (emacs-lisp-mode)
(setup (:pkg flycheck)
  (:hook-into lsp-mode emacs-lisp-mode)) ; TODO: better way to use flycheck



(provide 'ri-dev-gen)
;;; ri-dev-gen.el ends here
