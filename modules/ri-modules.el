;;; ri-modules --- general modules stuff -*- lexical-binding: t; -*-

;;; Commentary:
;; Organizes default modules to load and their order, and provides
;; relevant functions.

;;; Code:
(require 'cl-macs) ; for cl-typecase

(defvar ri/modules-pre-init-list
  '((require 'ri-package)
    (require 'ri-setup)
    (require 'ri-pivotal)))

(defvar ri/modules-init-list
  '((require 'ri-theme)
    (require 'ri-basic-ui)
    (require 'ri-basic-func)
    (require 'ri-def-fonts)))

;; (defvar ri/modules-features-list
;;   (mapcar ))

(defun ri/modules-require (list-or-type)
  "Evaluate require statements for loading modules.
If LIST-OR-TYPE is a list, eval all sublist exprs.
If LIST-OR-TYPE is a symbol, eval the associated module list variable.

Symbols:
- pre-init -> `ri/modules-pre-init-list'
- init -> `ri/modules-init-list'
- features -> `ri/modules-features-list'

More to be added..."
  (cl-typecase list-or-type
    (atom (ri/modules-require
	   (pcase 'pre-init
	     ('pre-init ri/modules-pre-init-list)
	     ('init ri/modules-init-list))))
    (list (mapcar (lambda (cmd)
		    (benchmark-progn
		      (eval cmd))
		    (message "  loaded %s" (nth 1 cmd)))
		  list-or-type))
    (t (error "Type not valid"))))

(provide 'ri-modules)
;;; ri-modules.el ends here
