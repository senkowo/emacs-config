;;; ri-emacs-lisp --- config for editing emacs lisp code -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
;; TODO: figure out how to make code blocks show color without scrolling up.

(require 'setup)

;;; --- Rainbow delimiters

(setup (:pkg rainbow-delimiters)
  (:hook-into prog-mode))


(provide 'ri-emacs-lisp)
;;; ri-emacs-lisp.el ends here
