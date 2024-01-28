;;; ri-dev-gen --- general config for editing code -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'setup)

(setup (:pkg flycheck)
  (:hook-into lsp-mode emacs-lisp-mode)) ; TODO: better way to use flycheck



(provide 'ri-dev-gen)
;;; ri-dev-gen.el ends here
