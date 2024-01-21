;;; ri-qol --- Quality Of Life improvements -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; --- Auto-revert changed files: ----

;; Update dired and file buffers when changed in filesystem.
(require 'autorevert)
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

;;; --- Make escape key escape: ----

;; By default, the <escape> key will actuate the Meta/Alt key.
;; This makes <escape> work more like expected.
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;; --- which-key / show possible keybinds: ----

;; If you enter C-x (example), it shows all possible keys for commands.
(setup (:pkg which-key)
  (which-key-mode 1)
  (setq which-key-idle-delay 0.3))


(provide 'ri-qol)
;;; ri-qol.el ends here
