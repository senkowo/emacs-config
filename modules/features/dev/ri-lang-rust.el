;;; ri-lang-rust --- rust n stuff -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Rust-analyzer required?, install as package before?

;;; Code:

(setup (:pkg rustic)
  (add-hook 'rust-mode-hook #'ri/lsp-start-appropriate-server)
  (add-hook 'after-init-hook
	    (lambda ()
	      (when (and (featurep 'tree-sitter)
			 (featurep 'tree-sitter-langs))
		;; fixes tree-sitter issue with rustic
		(tree-sitter-langs-install-grammars t)))))




(provide 'ri-lang-rust)
;;;; ri-lang-rust.el ends here
