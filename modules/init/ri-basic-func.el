;;; ri-basic-func --- set up basic functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(require 'setup)

;;; --- Load-paths: ----

(push (concat user-emacs-directory "lisp") load-path)

;;; --- TODO: load custom files in .emacs.d/lisp/... ---

;; { generic-functions, ri-git-interface-commands }.

;;; --- general.el: ----

;; TODO: make it compatible with evil-mode.
;; TODO: make EVERYTHING compatible with QWERTY!
(setup (:pkg general)
  (general-create-definer leader-key-def
    :prefix "C-c")
  (leader-key-def
    "s" '(:ignore t :which-key "special")
    "q" '(:ignore t :which-key "quit/session") ; TODO: relocate elsewhere
    "qq" '(save-buffers-kill-terminal :which-key "quit")))

;;; --- hydra: ----

(setup (:pkg hydra)
  ;; TODO: relocate both these, into better place.
  (defhydra hydra-text-scale (:timeout 5)
    "scale text"
    ("j" text-scale-decrease "out")
    ("k" text-scale-increase "in")
    ("f" nil "finished" :exit t))
  (leader-key-def
    "ss" '(hydra-text-scale/body :which-key "scale text")))

;;; --- Avy: ----

;; TODO: provide evil related stuff
(setup (:pkg avy)
  (leader-key-def
    "j" '(:ignore t :which-key "avy")
    "jj" 'avy-goto-char
    "jl" 'avy-goto-line))

;;; --- Save position in buffer: ----

(global-set-key (kbd "C-r") 'point-to-register)
(global-set-key (kbd "C-M-r") 'jump-to-register)

;;; --- basic custom keybinds: ----

;; scroll buffer up and down by one line:
(setq scroll-preserve-screen-position nil)
(global-set-key (kbd "M-n") (kbd "M-- 1 C-v"))
(global-set-key (kbd "M-p") (kbd "M-- 1 M-v"))

;; more comfortable key for complete-symbol for dvp
(global-set-key (kbd "C-M--") #'complete-symbol)



(provide 'ri-basic-func)
;;; ri-basic-func.el ends here
