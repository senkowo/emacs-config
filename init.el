;;; init.el --- main init file -*- lexical-binding: t; -*-

;;; Commentary:
;; The main init file.
;; There is another <file> for more user-side configuration.

;;; Code:

;; startup time
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "*** Emacs loaded in %s seconds with %d garbage collections."
		     (emacs-init-time "%.2f")
		     gcs-done)))

;; set gc during initialization
(setq gc-cons-threshold (* 50 1000 1000))

;; set gc after initialization
(add-hook 'after-init-hook (lambda ()
			     ;; Make gc pauses faster by decreasing the threshold.
                             (setq gc-cons-threshold (* 2 1000 1000))))

;; add modules to load-path
(defvar ri/load-path-modules nil)
;; TODO: recursively add all files from modules, that end with .el?
;;  get list of parent dirs for each file and get non-duplicating...
;;  Rewrite this with my own func, so can include everything!
(let ((default-directory (concat user-emacs-directory "modules"))
      (features-old features)) 
  (normal-top-level-add-subdirs-to-load-path)
  (dolist (i features)
    (unless (member i features-old)
      (cl-pushnew i ri/load-path-modules))))
;; (add-to-list 'load-path (concat user-emacs-directory "modules"))
;; (add-to-list 'load-path (concat user-emacs-directory "modules/pre-init"))
;; (add-to-list 'load-path (concat user-emacs-directory "modules/init"))
;; (add-to-list 'load-path (concat user-emacs-directory "modules/features"))
;; (add-to-list 'load-path (concat user-emacs-directory "modules/ri-config"))

;;; --- Identify System: ----

(defvar ri/is-linux (eq system-type 'gnu/linux))

(defvar ri/is-thinkpad (and ri/is-linux
                            (equal (system-name) "thinkpad1")))

;; use shell-command w/ grep to find "Guix System" in /etc/*release
(defvar ri/is-guix-system (and ri/is-linux
			       (file-exists-p "/etc/os-release")
                               (with-temp-buffer
                                 (insert-file-contents "/etc/os-release")
                                 (search-forward "ID=guix" nil t))
  			       t))

(defvar ri/has-guix-installed (and ri/is-linux
                                   (executable-find "guix")))

(defvar ri/exwm-enabled (and (pcase window-system
			       ('x t))
			     (or
			      (seq-contains-p command-line-args "--use-exwm")
			      (> (string-to-number
                                  (shell-command-to-string
                                   "ps aux | grep exwm | grep -vc grep"))
				 0))))

;;; -- Configuration Variables: ---

;;; -- Load Modules: ---
;; TODO: all the icons? nerd-fonts?
;; TODO: in order to test functionality, reorder the load order
;; of featured and ri-config to make sure it's order-independent.
;; TODO: define keyboard layout in var (dvp,qwerty,custom)
;; TODO: create macros that does Rest of modules, which can macroexpand.
;; TODO: running require causes errors to not show location of error...

(require 'ri-modules)

(defvar ri/pre-init-modules
  '((require 'ri-package)
    (require 'ri-setup)
    (require 'ri-pivotal)))
(ri/modules-require ri/pre-init-modules)

;; Load the user-side config
(load (concat user-emacs-directory "config"))






;;; init.el ends here

