;;; ri-lsp --- lsp-mode setup -*- lexical-binding: t; -*-

;;; Commentary:
;; 
;; Make sure clangd is found

;;; Code:

(require 'setup)

;; variables
(defvar ri/lsp-prefer-eglot-mode nil)

(defun ri/lsp-start-appropriate-server ()
  (if ri/lsp-prefer-eglot-mode
      (eglot-ensure)
    (lsp)))

;;; --- Lsp-mode: ----

(defvar ri/lsp-after-pkg-load-hook nil)

(setup (:pkg lsp-mode)
  ;; (require 'lsp-mode)
  (:option lsp-headerline-breadcrumb-enable t ; path at top
	   lsp-keymap-prefix "C-c l"
	   lsp-clients-clangd-executable (executable-find "clangd")
	   ;; reduce flashiness
	   lsp-eldoc-render-all nil	    ; all info in minibuffer
	   lsp-enable-symbol-highlighting t ; highlight same symbols
	   lsp-symbol-highlighting-skip-current t ; ^ dont highlight current symbol
	   ;;
	   lsp-inlay-hint-enable nil	; ?
	   ;; fixes yasnippet last bracket behavior
	   ;; (breaks auto-indent after enter in brackets?)
	   lsp-enable-relative-indentation t ; t fixes?
	   )
  (setq lsp-idle-delay 0.1)	  ; TODO: make mode/context/buffer sensitive (make alist?)

  ;; Enable Flycheck
  (add-hook 'lsp-mode-hook #'flycheck-mode)

  ;; TODO: do i need this?
  (:when-loaded
    ;; to get Meow space to work...
    ;; (maybe bc lsp is not deferred and run before meow?)
    (global-set-key (kbd "C-c l") lsp-command-map)
    (lsp-enable-which-key-integration 1)
    ;; Eval custom hook:
    (run-hooks 'ri/lsp-after-pkg-load-hook)
    )

  (leader-key-def
    "l" '(:ignore t :which-key "lsp")
    "lx" '(:ignore t :which-key "xref")
    "lxd" 'xref-find-definitions
    "lxr" 'xref-find-references
    "ln" 'lsp-ui-find-next-reference
    "lp" 'lsp-ui-find-prev-reference
    "ls" 'counsel-imenu
    "le" 'lsp-ui-flycheck-list
    "lS" 'lsp-ui-sideline-mode
    "lX" 'lsp-execute-code-action)
  )

;;; --- LSP-UI: ----

(setup (:pkg lsp-ui)
  (:hook-into lsp-mode)
  (:option lsp-ui-sideline-enable t ; sideline ?
	   lsp-ui-sideline-show-hover nil ; hover msgs in sideline ?
	   lsp-ui-doc-show-with-cursor nil ; show docs when hover with cursor
	   lsp-ui-doc-position 'bottom ; position of docs
	   )
  ;; TODO: no need since var already enabled?
  ;; (lsp-ui-doc-show 1)
  )

;;; --- Company Mode: ----

(defun ri/abort-company-and-meow-insert-exit ()
  "Abort company and exit meow insert mode."
  (interactive)
  (company-abort)
  (meow-insert-exit))

;; TODO: add keybind for yas-expand
(setup (:pkg company)
  ;; TODO: make it so it supports eglot (if i want company on eglot)
  (:hook-into lsp-mode)
  (:option company-minimum-prefix-length 2
	   company-idle-delay 0.1)
  (:with-map company-active-map
    (:bind "<tab>" company-complete-selection
	   "C-t" company-select-previous-or-abort
	   "C-<return>" company-select-next
	   "C-g" ri/abort-company-and-meow-insert-exit
	   "RET" nil))		     ; make RET not complete, only tab
  (:with-map company-search-map
    (:bind "C-t" company-select-previous-or-abort))

  ;; Use yasnippet as secondary company backend when lsp-mode is loaded:
  (defun ri/company-yasnippet-lsp-mode-config ()
    (setq lsp-completion-provider :none)
    (defun ri/company-lsp-backends ()
      (setq company-backends
	    '((company-capf :with company-yasnippet))))
    (add-hook 'lsp-mode-hook #'ri/company-lsp-backends))
  (add-hook 'ri/lsp-after-pkg-load-hook #'ri/company-yasnippet-lsp-mode-config)
  ;; (ri/run-func-if-feature-loaded 'lsp-mode #'ri/company-yasnippet-lsp-mode-config)

  ;; TODO: implement evil into this.
  )

;;; --- YASnippet: ----

;; TODO: fix issue with C yasnippet vs lsp completion different.
;; TODO: yasnippet completion

(setup (:pkg yasnippet)
  (require 'yasnippet)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (yas-reload-all)
  ;; TODO: if company installed,
  (define-key yas-keymap (kbd "C-<tab>") 'yas-next-field)
  )

(setup (:pkg yasnippet-snippets))

;; (setup (:pkg ivy-yasnippet))

;;; --- Projectile: ----

;;; --- Compilation: ----

(setup compile
  (setq compilation-scroll-output t))

;;; --- Eglot: ----

(setup (:pkg eglot))


(provide 'ri-lsp)
;;; ri-lsp.el ends here


