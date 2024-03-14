

;;; Commentary:
;; More info: https://emacs-tree-sitter.github.io/

;;; Code:

(unless (functionp 'module-load)
  (error "Emacs needs to be built with dynamic module support for tree-sitter"))

(setup (:pkg tree-sitter)
  ;; get langs
  (setup (:pkg tree-sitter-langs)
    (require 'tree-sitter-langs))
  
  (require 'tree-sitter)
  (global-tree-sitter-mode 1)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))



(provide 'ri-tree-sitter)
