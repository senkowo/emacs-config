

;;; Commentary:
;; More info: https://emacs-tree-sitter.github.io/
;; https://github.com/emacs-tree-sitter/elisp-tree-sitter

;; TODO: NOTE: treesitter uses different modes than original (e.g. c-mode-hook -> c-ts-mode-hook)

;;; Code:

;; (unless (functionp 'module-load)
;;   (error "Emacs needs to be built with dynamic module support for tree-sitter"))

;; Hack to get treesit to work properly:
;; https://github.com/renzmann/treesit-auto
;; (hack may become obsolete in Emacs 30)
;; If missing language grammars, run treesit-auto-install-all.
(setup (:pkg treesit-auto)
  (require 'treesit-auto)
  (setq treesit-auto-install 'prompt)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode 1))

;; Original method (has issues with rust-mode):

;; (setup (:pkg tree-sitter)
;;   (require 'tree-sitter))
;; (setup (:pkg tree-sitter-langs)
;;   (:load-after tree-sitter)
;;   (require 'tree-sitter-langs)
;;   (global-tree-sitter-mode 1)
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))


(provide 'ri-tree-sitter)
