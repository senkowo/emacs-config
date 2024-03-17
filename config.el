;;; config.el --- a simple interface to configure Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Notes:
;; - use setup macros to choose not to load if not enabled here.
;;

;;; Code:

(require 'setup)


;;; --- Fonts: ----
;;
;; Set your fonts here.
;; These make sure these fonts can be found.
;; Run this command in terminal to check if font can be found by emacs:
;; <insert command here>
;; Alternatively the command "fc-list" and search for the font name.
;;
;; Note:
;; The fonts here other than the default-font don't really do anything
;; on their own. But in ri-org for example, certain font faces are set to
;; use the fixed-pitch font instead of the default, and so on...
;;
;; Run the following to show all available fonts verbosely:
;; (x-list-fonts "*" nil frame)

(defvar font-alist
  '((tamzen-20 . "-Misc-Tamzen-regular-normal-normal-*-20-*-*-*-c-100-iso10646-1")
    (tamzen-16 . "-Misc-Tamzen-regular-normal-normal-*-16-*-*-*-c-80-iso10646-1")
    (tamzenPL-16 . "-Misc-TamzenForPowerline-regular-normal-normal-*-16-*-*-*-c-80-iso10646-1")
    (terminus-16 . "-xos4-Terminus-regular-normal-normal-*-16-*-*-*-c-80-iso10646-1")
    (spleen-16 . "-misc-Spleen-regular-normal-normal-*-16-*-*-*-m-*-iso10646-1")
    (terminal-20 . "-dylex-terminal-regular-normal-normal-*-20-*-*-*-c-100-iso10646-1")
    (unscii-16 . "-Unscii-Unscii-medium-normal-normal-16-16-*-*-*-c-80-iso10646-1")
    (fixed-16 . "-*-fixed-medium-r-normal-*-16-*-*-*-*-*-fontset-standard")))

;;; Default Font:
;; typically for regular text, modeline, minibuffer, etc.
;; TODO: why defvar here????
;; (set-face-attribute 'default nil :font "Fira Code" :height 110)
;; (set-face-attribute 'default nil :font "JetBrains Mono" :height 115)
;; (set-face-attribute 'default nil :font "Hack" :height 110)
;; (set-face-attribute 'default nil :font "Borg Sans Mono" :height 120) ; old (s/n)
;; (set-face-attribute 'default nil :font "Hermit" :height 110) ; lain-ish (s/n) ; nice
;; (set-face-attribute 'default nil :font "Iosevka" :height 120)
;; (set-face-attribute 'default nil :font "Iosevka Comfy" :height 120) ; gud
;; (set-face-attribute 'default nil :font "Iosevka Comfy Fixed" :height 120)
;; Bitmap:
;; (set-face-attribute 'default nil :font (alist-get 'tamzen-20 font-alist))
;; (set-face-attribute 'default nil :font (alist-get 'terminal-20 font-alist))

(set-face-attribute 'default nil :font (alist-get 'tamzenPL-16 font-alist))
;; (set-face-attribute 'default nil :font (alist-get 'terminus-16 font-alist))

;; (set-face-attribute 'default nil :font (alist-get 'spleen-16 font-alist))
;; (set-face-attribute 'default nil :font (alist-get 'unscii-16 font-alist))
;; (set-face-attribute 'default nil :font (alist-get 'fixed-16 font-alist))


;;; Fixed Pitch Font:
;; typically for code blocks, org-attributes, etc.
;; (set-face-attribute 'fixed-pitch nil :font "Fira Code" :height 110)
(set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :height 110)

;; non-monospaced, typically for org-mode with variable-pitch-mode, etc.
;; (set-face-attribute 'variable-pitch nil :font "DejaVu Sans" :height 125 :weight 'regular)
(set-face-attribute 'variable-pitch nil :font "JetBrains Mono" :height 110 :weight 'regular)


;;; --- Modules: ----

(require 'ri-modules)

;; This shouldn't be edited unless u know what ur doing.
;; (must be in this order
(defvar ri/init-modules
  '((require 'ri-theme)
    (require 'ri-basic-ui)
    (require 'ri-basic-func)
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
    (require 'ri-lsp)
    (require 'ri-lang-c-cpp)
    (require 'ri-lang-cl)
    (require 'ri-lang-scheme)
    (require 'ri-lang-rust)
    (require 'ri-tree-sitter)
    ;; other
    (require 'ri-server)))
(ri/modules-require ri/general-modules)

;;; ----- Do whatever you want below: -----


;;; --- Fonts: ----


(defvar ri/org-use-variable-pitch t
  "Use variable pitch fonts for org.")

(defvar ri/use-standard-config-modules nil
  "If non-nil, use a generic config of modules below.")

;; start emacs with the argument "--sane-keybinds" to set the var to t
(if (seq-contains command-line-args "--sane-keybinds")
    (setq ri/use-standard-config-modules t))

(if ri/use-standard-config-modules
    ;; default config modules. set up for meow with qwerty:
    (progn
      (defvar rx/example-meow-qwerty-modules
	'((require 'ri-meow-qwerty)))
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
