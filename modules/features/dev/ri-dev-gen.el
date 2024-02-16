;;; ri-dev-gen --- general config for editing code -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'setup)

;; NOTE: this will show a bunch of error messages when editing lisp code (emacs-lisp-mode)
;; TODO: disable flycheck on elisp, bc it demands docs?
;; Modes to enable flycheck in are enabled in their respective configs.
(setup (:pkg flycheck))



(provide 'ri-dev-gen)
;;; ri-dev-gen.el ends here
