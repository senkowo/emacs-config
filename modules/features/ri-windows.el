;;; ri-windows --- emacs window management -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'setup)

;;; --- Window auto balance after splitting: ----

(defun ri/delete-window-auto-balance ()
  "Delete currentt window, then balance windows."
  (interactive)
  (delete-window)
  (balance-windows))

(defun ri/split-window-right-auto-balance ()
  "Split window to the right, then balance windows."
  (interactive)
  (split-window-right)
  (balance-windows))

(defun ri/split-window-below-auto-balance ()
  "Split window below, then balance windows."
  (interactive)
  (split-window-below)
  (balance-windows))

(global-set-key (kbd "C-x 0") #'ri/delete-window-auto-balance)
(global-set-key (kbd "C-x 2") #'ri/split-window-below-auto-balance)
(global-set-key (kbd "C-x 3") #'ri/split-window-right-auto-balance)

;;; --- Ace window: ----

(setup (:pkg ace-window)
  (setq aw-scope 'frame)
  (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?g ?c ?r)) ; TODO: qwerty variant
  (defvar aw-dispatch-alist ; Maybe just move all these into qwerty...?...
    '((?d aw-delete-window "Delete Window")
      (?1 delete-other-windows "Delete Other Windows")
      (?s aw-split-window-horz "Split Horz Window")
      (?v aw-split-window-vert "Split Vert Window")
      (?, aw-split-window-fair "Split Fair Window")
      (?o aw-flip-window "Other Window")
      (?w aw-swap-window "Swap Windows")
      (?m aw-move-window "Move Window")
      (?c aw-copy-window "Copy Window")
      (?b aw-switch-buffer-in-window "Select Buffer")
      (?B aw-switch-buffer-other-window "Switch Buffer Other Window")
      (?? aw-show-dispatch-help))
    "List of actions for `aw-dispatch-default'.")
  (global-set-key (kbd "M-o") 'ace-window))






(provide 'ri-windows)
;;; ri-windows.el ends here
