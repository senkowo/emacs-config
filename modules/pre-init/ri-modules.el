;;; ri-modules --- general modules stuff -*- lexical-binding: t; -*-

;;; Commentary:
;; Organizes default modules to load and their order, and provides
;; relevant functions.

;;; Code:
(require 'cl-macs) ; for cl-typecase

;;; --- Modules variables: ----

;; TODO: is this necessary?

(defvar ri/modules-pre-init-list
  ;; '((require 'ri-package)
  ;;   (require 'ri-setup)
  ;;   (require 'ri-pivotal))
  '((ri/load "ri-package")
    (ri/load "ri-setup")
    (ri/load "ri-pivotal")))

(defvar ri/modules-init-list
  ;; '((require 'ri-theme)
  ;;   (require 'ri-basic-ui)
  ;;   (require 'ri-basic-func)
  ;;   (require 'ri-def-fonts)
  ;;   (require 'ri-qol))
  '((ri/load "ri-theme")
    (ri/load "ri-basic-ui")
    (ri/load "ri-basic-func")
    (ri/load "ri-def-fonts")
    (ri/load "ri-qol")))

;; (defvar ri/modules-features-list
;;   (mapcar ))

;;; --- Custom benchmarking tool: ----

;;; --- Modules require wrapper: ----

(defun ri/modules-require (list-or-type)
  "Evaluate require statements for loading modules.
If LIST-OR-TYPE is a list, eval all sublist exprs.
If LIST-OR-TYPE is a symbol, eval the associated module list variable.

Symbols:
- pre-init -> `ri/modules-pre-init-list'
- init -> `ri/modules-init-list'
- features -> `ri/modules-features-list'

More to be added..."
  (message "DEBUG: REMOVE THIS")
  (cl-typecase list-or-type
    (atom (ri/modules-require		; TODO: what. recursion...?
	   (pcase 'pre-init
	     ('pre-init ri/modules-pre-init-list)
	     ('init ri/modules-init-list))))
    (list (mapcar (lambda (cmd)
		    (benchmark-progn
		      (eval cmd))
		    ;; (message "  loaded %s" (nth 1 cmd))
		    )
		  list-or-type))
    (t (error "Type not valid"))))




(defmacro ri/load (&rest modules)
  `(progn
     ,@(mapcar (lambda (arg)
		 ;; for every arg passed to macro, if arg is a variable
		 ;; name, 
		 (cl-typecase arg
		   (string `(progn
			      (benchmark-progn
				(load ,arg))
			      ;; (message "  loaded %s" ,arg)
			      ))
		   (atom `(progn
			    ,@(mapcar (lambda (exp)
					exp)
				      (symbol-value arg))))
		   (t (error "invalid type: %s" arg))))
	       modules)))

;;; --- Eval function after init if package exists

(defun ri/run-func-after-init (my-func)
  "Once Emacs is initialized, eval MY-FUNC.
If ran during Emacs initialization, evaluate in after-init-hook.
If already initialized, evaluate now."
  (if after-init-time
      (funcall my-func)
    (add-hook 'after-init-hook my-func)))

(defun ri/run-func-if-feature-loaded (my-feat my-func)
  "Once Emacs is initialized, evals MY-FUNC if FEATURE is loaded.
If ran during Emacs initialization, evaluate in after-init-hook.
If already initialized, evaluate now."
  (cond (after-init-time
	 (if (featurep my-feat)
	     (funcall my-func)))
	(t
	 (add-hook 'after-init-hook
		   (lambda ()
		     (if (featurep my-feat)
			 (funcall my-func)))))))

;;; --- End: ----

(provide 'ri-modules)
;;; ri-modules.el ends here
