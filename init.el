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

;; gc during initialization
(setq gc-cons-threshold (* 50 1000 1000))

;; gc after initialization
(add-hook 'after-init-hook (lambda ()
			     ;; Make gc pauses faster by decreasing the threshold.
                             (setq gc-cons-threshold (* 2 1000 1000))))

;; add modules to load-path
(add-to-list 'load-path (concat user-emacs-directory "modules"))
(add-to-list 'load-path (concat user-emacs-directory "modules/pre-init"))
(add-to-list 'load-path (concat user-emacs-directory "modules/init"))
(add-to-list 'load-path (concat user-emacs-directory "modules/features"))
(add-to-list 'load-path (concat user-emacs-directory "modules/ri-config"))

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

(require 'ri-modules)



(defvar ri/init-modules-list nil)
(setq ri/init-modules-list
      '(;; pre-init (need to be in this order)
	(require 'ri-package)
	(require 'ri-setup)
	(require 'ri-pivotal)

	;; init
	;; (process and prioritize user settings from here)
	;; (things that MUST come before everything else)
	;; TODO: make it so you can do (r ...) (r ...) 'rest
	(require 'ri-theme)
	(require 'ri-basic-ui)
	(require 'ri-basic-func)
	(require 'ri-def-fonts)
	(require 'ri-qol)

	;; features
	;; (here, order is irrelevant).
	(require 'ri-line-numbers)
	(require 'ri-core)		; empty right now
	(require 'ri-org)
	(require 'ri-transparency)
	(require 'ri-meow-gen) ; T
	;; (require 'ri-evil-keys) ; T
	(require 'ri-windows)
	(require 'ri-buffers)

	;; my-config
	(require 'ri-meow-dvp)


	
	;; (require 'ri-exwm)

	(require 'ri-server)

	))

(mapc (lambda (cmd)
	(benchmark-progn
	  (eval cmd))
	(message "  loaded %s" (nth 1 cmd)))
      ri/init-modules-list)

;;; init.el ends here

