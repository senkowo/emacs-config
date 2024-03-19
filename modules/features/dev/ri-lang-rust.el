;;; ri-lang-rust --- rust n stuff -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Rust-analyzer required?, install as package before?

;;; Code:

;; Docs: (TODO: add more stuff)
;; https://github.com/brotzeit/rustic
(setup (:pkg rustic)
  ;; (add-hook 'rust-mode-hook #'ri/lsp-start-appropriate-server) ; no need, automatic
  (setq rustic-lsp-client 'lsp-mode)) ; def

;; TODO: option without rustic:
;; (setup (:pkg rust-mode))
;; (setup (:pkg cargo-mode))


;; TODO: FOUND!!!
;;  LAZY TESTING:
;; (require 'cl-macs) ; unnecessary? overhead? only require if not feature-p???
;; (cl-flet
;;     ((do-stuff ()
;;        (stuff)))
;;   (if (emacs-is-finished-init)
;;       (do-stuff)
;;     (add-hook 'after-init-hook #'do-stuff)))

(provide 'ri-lang-rust)
;;;; ri-lang-rust.el ends here
