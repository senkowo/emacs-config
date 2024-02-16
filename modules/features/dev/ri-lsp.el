;;; ri-lsp --- lsp-mode setup -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'setup)

(setup (:pkg lsp-mode)
  (:option lsp-headerline-breadcrumb-enable t ; path at top
	   lsp-keymap-prefix "C-c l"
	   lsp-clients-clangd-executable (executable-find "clangd")
	   ;; reduce flashiness
	   lsp-eldoc-render-all nil ; all info in minibuffer
	   lsp-enable-symbol-highlighting t ; highlight same symbols
	   lsp-symbol-highlighting-skip-current t ; ^ dont highlight current symbol
	   ;;
	   lsp-inlay-hint-enable nil ; ?
	   ;; fixes yasnippet last bracket behavior
	   ;; (breaks auto-indent after enter in brackets?)
	   lsp-enable-relative-indentation t ; t fixes?
	   )
  (setq lsp-idle-delay 0.1) ; make mode/context/buffer sensitive

  ;; Enable Flycheck
  (add-hook 'lsp-mode-hook #'flycheck-mode)

  ;; TODO: do i need this?
  (:when-loaded
    ;; to get Meow space to work...
    ;; (maybe bc lsp is not deferred and run before meow?)
    (global-set-key (kbd "C-c l") lsp-command-map)
    (lsp-enable-which-key-integration 1))

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

(setup (:pkg company)
  ;; TODO: make it so it supports eglot (if i want company on eglot)
  (:hook-into lsp-mode)
  (:option company-minimum-prefix-length 1
	   company-idle-delay 0.1)
  (:with-map company-active-map
    (:bind "<tab>" company-complete-selection
	   "C-t" company-select-previous-or-abort
	   "C-<return>" company-select-next
	   "C-g" ri/abort-company-and-meow-insert-exit
	   "RET" nil)) ; make RET not complete, only tab
  (:with-map company-search-map
    (:bind "C-t" company-select-previous-or-abort))
  ;; TODO: implement evil into this.
  )

;;; --- YASnippet: ----

(setup (:pkg yasnippet)
  (require 'yasnippet)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (yas-reload-all))

(setup (:pkg yasnippet-snippets))

;; (setup (:pkg ivy-yasnippet))

;;; --- Projectile: ----

;;; --- Compilation: ----

(setup compile
  (setq compilation-scroll-output t))


(provide 'ri-lsp)
;;; ri-lsp.el ends here
