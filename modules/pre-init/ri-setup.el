;;; ri-setup --- setup setup.el setup macro -*- lexical-binding: t; mode: emacs-lisp; mode: outline-minor; -*-

;;; Commentary:
;; setup.el is an alternative to use-package, and is the primary setup
;; macro used in this configuration.

;;; Code:

;;; --- setup.el ----

;; [[https://www.emacswiki.org/emacs/SetupEl][setup.el - Emacs Wiki]]
;; Keywords to note:
;; - :when-loaded (basically =:config= I think) (apparently should try
;; to avoid using...) (=require= also loads package and achieves
;; similar functionality.)
;; - :load-after /package/ (load this after /package/ loads).
;; - :hook /mode/ : add /mode/ to current package's hook.
;; - :hook-into /mode/ : hook current package into /mode/'s
;; (e.g. (visual-fill-column (:hook-into org-mode)))

(unless (fboundp 'straight-use-package)
  (error "Cannot install setup.el bc straight is not installed - rin"))
(straight-use-package '(setup :type git :host nil :repo "https://git.sr.ht/~pkal/setup"))
(require 'setup)

;;; ---- :pkg -----

;; Recipe is always a list
;; Install via Guix if length == 1 or :guix t is present

(defvar ri/guix-emacs-packages '()
  "Contains a list of all Emacs package names that must be installed via Guix.")

;; Examples:
;; - (org-roam :straight t)
;; - (git-gutter :straight git-gutter-fringe)

(defun ri/filter-straight-recipe (recipe)
  "Return straight recipe from name RECIPE."
  (let* ((plist (cdr recipe))
         (name (plist-get plist :straight)))
    (cons (if (and name (not (equal name t)))
              name
            (car recipe))
          (plist-put plist :straight nil))))

(setup-define :pkg
  (lambda (&rest recipe)
    (if (and ri/is-guix-system	      ; modified, but will this break?
             nil		      ; DISABLE
             (or (eq (length recipe) 1)
                 (plist-get (cdr recipe) :guix)))
        ;; if ri/is-guix-system and regular input, install w/ guix.
        (progn `(add-to-list 'ri/guix-emacs-packages
                             ,(or (plist-get recipe :guix)
                                  (concat "emacs-" (symbol-name (car recipe))))))
      ;; else, install directly with straight.el
      `(straight-use-package ',(ri/filter-straight-recipe recipe))))
  :documentation "Install RECIPE via Guix or straight.el"
  :shorthand #'cadr)

;;; ---- :delay -----

;; Delay the loading of a package until a certain amount of idle time has passed.
(setup-define :delay
  (lambda (&rest time)
    `(run-with-idle-timer ,(or time 1)
                          nil ;; Don't repeat
                          (lambda () (require ',(setup-get 'feature)))))
  :documentation "Delay loading the feature until a certain amount of idle time has passed.")

;;; ---- :disabled -----

;; Disable a package configuration.
(setup-define :disabled
  (lambda ()
    `,(setup-quit))
  :documentation "Always stop evaluating the body.")

;;; ---- :load-after -----

;; Execute body after package loads (:after in use-package).
(setup-define :load-after
  (lambda (features &rest body)
    (let ((body `(progn
                   (require ',(setup-get 'feature))
                   ,@body)))
      (dolist (feature (if (listp features)
                           (nreverse features)
                         (list features)))
        (setq body `(with-eval-after-load ',feature ,body)))
      body))
  :documentation "Load the current feature after FEATURES."
  :indent 1)


(provide 'ri-setup)
;;; ri-setup.el ends here
