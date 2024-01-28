;;; ri-lisp-adv --- advanced lisp editing stuff -*- lexical-binding: t; -*-

;;; Commentary:

;; Note: paredit is confusing for beginners!

;;; Code:

(require 'setup)

;;; --- Paredit: ----

(setup (:pkg paredit)
  (:hook-into emacs-lisp-mode scheme-mode)
  (:bind "M-r" nil)) ; originally `paredit-raise-sexp'

;; TODO: provide lispy for evil-mode

(provide 'ri-lisp-adv)
;;; ri-lisp-adv.el ends here
