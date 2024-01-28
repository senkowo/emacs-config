;;; config.el --- a simple interface to configure Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Notes:
;; - use setup macros to choose not to load if not enabled here.
;;

;;; Code:

(require 'setup)
(require 'ri-modules)

;; This shouldn't be edited unless u know what ur doing.
;; (must be in this order
(defvar ri/init-modules
  '((require 'ri-theme)
    (require 'ri-basic-ui)
    (require 'ri-basic-func)
    (require 'ri-def-fonts)
    (require 'ri-qol)))
(ri/modules-require ri/init-modules)

;; List of modules to load. The order of modules shouldn't matter.
(defvar ri/general-modules
  '(;; general
    (require 'ri-line-numbers)
    (require 'ri-core)			; empty right now
    (require 'ri-org)
    (require 'ri-transparency)
    (require 'ri-meow-gen)		; T
    ;; (require 'ri-evil-keys) ; T
    (require 'ri-windows)
    (require 'ri-buffers)
    (require 'ri-tools)
    (require 'ri-modeline)
    (require 'ri-qol2)
    (require 'ri-completion-ivy)
    (require 'ri-pretty)
    (require 'ri-fun)
    (require 'ri-guix)
    (require 'ri-terminal)
    (require 'ri-dired)
    (require 'ri-dirvish)

    ;; dev
    (require 'ri-magit)
    (require 'ri-emacs-lisp)
    (require 'ri-lisp-gen)
    (require 'ri-lisp-adv)
    (require 'ri-dev-gen)

    ;; other
    (require 'ri-server)))
(ri/modules-require ri/general-modules)

;;; ----- Do whatever you want below: -----

(defvar ri/use-standard-config-modules nil
  "If non-nil, use a generic config of modules below.")

;; start emacs with the argument "--sane-keybinds" to set the var to t
(if (seq-contains command-line-args "--sane-keybinds")
    (setq ri/use-standard-config-modules t))

(if ri/use-standard-config-modules
    ;; default config modules. set up for meow with qwerty:
    (progn
      (defvar rx/example-meow-qwerty-modules
	'(require 'ri-meow-qwerty))
      (ri/modules-require rx/example-meow-qwerty-modules))
  
  ;; Otherwise, use my personal config:
  ;; Loads a list of modules suited for me, lily :3 (mostly just dvp keybinds).
  ;; (comment this out if u dont want meow with dvorak)
  (progn
    (defvar ri/lily-specific-modules
      '((require 'ri-meow-dvp)
	(require 'ri-swap-x-and-u)))
    (ri/modules-require ri/lily-specific-modules)))





;;; config.el ends here
