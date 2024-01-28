;;; ri-qol --- Quality Of Life improvements -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'setup)

;;; --- Auto-revert changed files: ----

;; Update dired and file buffers when changed in filesystem.
(require 'autorevert)
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

;;; --- Make escape key escape: ----

;; By default, the <escape> key will actuate the Meta/Alt key.
;; This makes <escape> work more like expected.
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;; --- which-key: ----

;; Show possible keybinds when hitting a prefix key.

;; If you enter C-x (example), it shows all possible keys for commands.
(setup (:pkg which-key)
  (diminish 'which-key-mode)
  (setq which-key-idle-delay 0.3)
  (which-key-mode 1))

;;; --- Leader key defs: ----

(leader-key-def
  "f"  '(:ignore t :which-key "files")
  "fr" '(recentf :which-key "recent files")
  "ff" '(find-file :which-key "find-file"))


(provide 'ri-qol)
;;; ri-qol.el ends here
