;;; ri-terminal --- terminal stuffs -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'setup)
(require 'hydra)


;;; --- Vterm: ----

;; hydra shortcut to later be used
(defhydra ri/hydra-vterm-scale (:timeout 5)
  "scale text"
  ("+" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("t" text-scale-increase "in")
  ("n" text-scale-decrease "out")
  ("=" text-scale-set "reset")
  ("g" nil "finished" :exit t))

;; configure vterm
(setup (:pkg vterm)
  (:with-map vterm-mode-map
    (:bind "C-," vterm-send-next-key
           "C-=" ri/hydra-vterm-scale/body
           "C--" text-scale-decrease
           "C-+" text-scale-increase))
  (:when-loaded
    ;; enter Meow insert mode automatically when open
    ;; TODO: add no meow and vim variant!
    (when meow-global-mode
      (add-hook 'vterm-mode-hook (lambda ()
                                   (if meow-normal-mode
                                       (meow-insert-mode)))))
    ;; v already set v
    ;; (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
    (setq vterm-shell "bash")
    (setq vterm-buffer-name-string "vterm %s")
    (setq vterm-max-scrollback 10000)
    ;; Fixes vterm compilation on Guix System.
    ;; (https://www.reddit.com/r/GUIX/comments/11gzhyu/
    ;;  how_to_compile_the_vterm_module_from_emacs_and/)
    (defun ri/vterm-link-guix-library-on-compile (f &rest r)
      "Advice to replace compiling vterm with linking to just symlinking the guix library"
      (let ((guix-library "~/.guix-extra-profiles/emacs/emacs/lib/vterm-module.so"))
        (if (f-exists-p guix-library)
            (make-symbolic-link
             (expand-file-name "~/.guix-extra-profiles/emacs/emacs/lib/vterm-module.so")
             (file-name-directory (locate-library "vterm.el" t)) t)
          (message "DEBUG: vterm guix library %s doesn't exist, cant compile" guix-library))))
    (if ri/is-guix-system ; TODO: no need to do this shit :sob::sob::sob:
        (advice-add 'vterm-module-compile :around #'ri/vterm-link-guix-library-on-compile))))

;;; --- Multi-vterm: ----

(setup (:pkg multi-vterm)
  (setq multi-vterm-dedicated-window-height-percent 30)
  (leader-key-def))

;;; leader keys: ;;;

(leader-key-def
  "at" 'vterm ; regular term
  "aT" 'multi-vterm ; create new multi-vterm
  "am" '(:ignore t :which-key "multi-vterm-control") ; keymap
  "amt" 'multi-vterm-project
  "amp" 'multi-vterm-prev
  "amn" 'multi-vterm-next
  "amm" 'multi-vterm-dedicated-toggle)


(provide 'ri-terminal)
;;; ri-terminal.el ends here
