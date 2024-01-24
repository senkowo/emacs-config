;;; ri-buffers --- emacs buffer management -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'setup)

;;; --- Buffer leader keys: ----

(leader-key-def
  "k" 'kill-current-buffer
  "b" '(:ignore t :which-key "buffer")
  "bk" 'kill-current-buffer
  "bn" 'next-buffer
  "bp" 'previous-buffer
  "bo" 'meow-last-buffer
  "bb" 'counsel-switch-buffer
  "br" 'read-only-mode)

(provide 'ri-buffers)
;;; ri-buffers.el ends here
