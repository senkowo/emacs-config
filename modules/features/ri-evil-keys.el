;;; ri-evil-keys.el --- evil-mode config -*- lexical-binding: t; -*-

;;; Commentary:
;; Have to make cooperate with general.el
;; TODO: make it compatible with general.el
;;  (enable some options along with this stuff)

;;; Code:

(require 'setup)

(defun ri/evil-hook ()
  "The modes to exclude from evil-mode.
TODO: later make this a var, so that it's easier to customize."
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  erc-mode
                  circe-server-mode
                  circe-chat-mode
                  circe-query-mode
                  sauron-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

;; evil-mode
(setup (:pkg evil)
  (setq evil-want-keybinding nil)
  (require 'evil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  (setq evil-want-integration t)
  (setq evil-undo-system 'undo-tree)
  (add-hook 'evil-mode-hook 'ri/evil-hook)
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join) ; wowie

  ;; Use visual line motions even outside of visual-line-mode buffers
  ;; -- haven't set up visual line mode yet
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))
;;hook
;; have these programs be in emacs-mode (C-z)
;;(evil-mode-hook . mi/evil-hook)

;; evil keybinds for even more modes
(setup (:pkg evil-collection)
  (evil-collection-init))


(provide 'ri-evil-keys)
;;; ri-evil-keys.el ends here
