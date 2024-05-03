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

;; TODO: test what happens if no fonts are set by default.

;; TODO: italics for bitmap fonts

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
(set-face-attribute 'default nil :font "Fira Code" :height 110)
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

;; DOESNT WORK (set-face-attribute 'default nil :font (alist-get 'tamzenPL-16 font-alist))
;; (set-face-attribute 'default nil :font (alist-get 'terminus-16 font-alist))

;; (set-face-attribute 'default nil :font (alist-get 'spleen-16 font-alist))
;; (set-face-attribute 'default nil :font (alist-get 'unscii-16 font-alist))
;; (set-face-attribute 'default nil :font (alist-get 'fixed-16 font-alist))

;;; Bitmap italic exception:
(set-face-attribute 'italic nil :font "Hack")


;;; Fixed Pitch Font:
;; typically for code blocks, org-attributes, etc.
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height 110)
;; (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :height 110)

;;; Variable Pitch Font:
;; (set-face-attribute 'variable-pitch nil :font "DejaVu Sans" :height 125 :weight 'regular)
;; (set-face-attribute 'variable-pitch nil :font "JetBrains Mono" :height 110 :weight 'regular)
(set-face-attribute 'variable-pitch nil :font "Fira Code" :height 110)


;;; --- Modules: ----

;; (require 'ri-modules)
;; (load "ri-modules")

;; This shouldn't be edited unless u know what ur doing.
;; (must be in this order
(defvar ri/init-modules
  '((ri/load "ri-theme")
    (ri/load "ri-basic-ui")
    (ri/load "ri-basic-func")
    (ri/load "ri-qol")))
(ri/load ri/init-modules) ; TODO: rename to ri/modules-require-list for clarity

;;; List of modules to load. The order of modules shouldn't matter.

;; general


;; TODO: replace all these with #'load, or a function that individually processes each string and loads, with timestamps and shit.


(ri/load "ri-line-numbers")
(ri/load "ri-core")			; empty right now
(ri/load "ri-org")
(ri/load "ri-transparency")
(ri/load "ri-meow-gen")		; T
;; (require 'ri-evil-keys) ; T
(ri/load "ri-windows")
(ri/load "ri-buffers")
(ri/load "ri-tools")
(ri/load "ri-modeline")
(ri/load "ri-qol2")
(ri/load "ri-completion-ivy")
(ri/load "ri-pretty")
(ri/load "ri-fun")
(ri/load "ri-guix")
(ri/load "ri-terminal")
(ri/load "ri-dired")
(ri/load "ri-dirvish")

;; dev
(ri/load "ri-magit")
(ri/load "ri-emacs-lisp")
(ri/load "ri-lisp-gen")
(ri/load "ri-lisp-adv")
(ri/load "ri-dev-gen")
(ri/load "ri-lsp")
(ri/load "ri-lang-c-cpp")
(ri/load "ri-lang-cl")
(ri/load "ri-lang-scheme")
;; (require 'ri-tree-sitter) ; broken rust comments
(ri/load "ri-lang-rust")
;; other
(ri/load "ri-server")
;; misc
(ri/load "ri-rest") ; temp

;;; ----- Do whatever you want below: -----

;; TODO: add to docs, to enable debug-on-error for debugging or --init-debug
;; (setq debug-on-error t) ; errors when ) end of list


;; TODO: how to show error message with file/location of error, not the parent file (or something).
(if (featurep 'ri-org)
    (defvar ri/org-use-variable-pitch t
      "Use variable pitch fonts for org."))

(defvar ri/use-standard-config-modules nil
  "If non-nil, use a generic config of modules below.")

;; start emacs with the argument "--sane-keybinds" to set the var to t
(if (seq-contains command-line-args "--sane-keybinds")
    (setq ri/use-standard-config-modules t))

(if ri/use-standard-config-modules
    ;; default config modules. set up for meow with qwerty:
    (progn
      (defvar rx/example-meow-qwerty-modules
	'((ri/load "ri-meow-qwerty")))
      (ri/modules-require rx/example-meow-qwerty-modules))
  
  ;; Otherwise, use my personal config:
  ;; Loads a list of modules suited for me, lily :3 (mostly just dvp keybinds).
  ;; (comment this out if u dont want meow with dvorak)
  (progn
    (defvar ri/lily-specific-modules
      '((ri/load "ri-meow-dvp")
	(ri/load "ri-swap-x-and-u")))
    (ri/modules-require ri/lily-specific-modules)))




;;; config.el ends here
