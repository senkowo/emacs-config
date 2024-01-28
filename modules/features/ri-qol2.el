;;; ri-qol2 --- more quality of life changes -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'setup)

;;; --- Helpful: ----

;; TODO: does this depend on counsel? will i use something else? YES!!!
;; Adds set, manual, references, source code, etc.

(setup (:pkg helpful)
  (:option counsel-describe-function-function #'helpful-callable
           counsel-describe-variable-function #'helpful-variable)
  (:global [remap describe-function] helpful-function
           [remap describe-symbol] helpful-symbol
           [remap describe-variable] helpful-variable
           [remap describe-command] helpful-command
           [remap describe-key] helpful-key
	   "C-h h" helpful-at-point
	   "C-h M" which-key-show-major-mode
	   "C-h E" describe-keymap)) ; TODO: add leader-key-versions of funcs?



(provide 'ri-qol2)
;;; ri-qol2.el ends here
