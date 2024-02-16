;;; ri-org --- configure org-mode -*- lexical-binding: t; mode: emacs-lisp; mode: outline-minor; -*-

;;; Commentary:

;;; Code:

(require 'setup)
(require 'org-indent)

;;; --- Org basic setup: ----

(defun ri/org-buffer-setup ()
  "Should be run when opening new org buffer."
  ;; (variable-pitch-mode 1)
  (visual-line-mode 1) ; wrap lines
  )

(setup org
  (:hook ri/org-buffer-setup)
  (:option org-ellipsis " ▼"
           org-hide-emphasis-markers t ; hide formatting chars (* / ~ = etc)
	   org-src-preserve-indentation t ; no space at front of code blocks.
	   org-src-window-setup 'current-window ; how to open edit code block.
	   org-startup-indented t ; indent content to heading level.
	   org-indent-mode-turns-on-hiding-stars t ; hide stars when org-indent-mode
           )
  (:with-map org-mode-map
    (defun ri/org-insert-subheading-respect-content ()
      (interactive)
      (org-insert-heading-respect-content)
      (org-demote-subtree))
    (:bind "C-M-<return>" ri/org-insert-subheading-respect-content)))

;;; --- Org fonts: ----

(require 'org)
(defun ri/org-font-setup ()
  "Function that set up org font settings."
  (interactive)
  (set-face-attribute 'org-document-title nil :weight 'bold :height 1.5)
  (dolist (face '((org-level-1 . 1.3)
                  (org-level-2 . 1.25)
                  (org-level-3 . 1.2)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    ;; font for bullets
    (set-face-attribute (car face) nil :font "Liberation Mono" :weight 'bold :height (cdr face))
    ;; (set-face-attribute (car face) nil :font "" :weight 'bold :height (cdr face))
    )

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch :height 1.5)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)

  (set-face-attribute 'org-hide nil :inherit 'fixed-pitch) ; fixes indentation

  ;; more options that can be set:
  (set-face-attribute 'org-tag nil :inherit '(shadow) :weight 'bold)
  ;; (set-face-attribute 'org-document-info :foreground "dark orange")
  ;; (set-face-attribute 'org-document-info-keyword :inherit (shadow fixed-pitch))
  ;; (set-face-attribute 'org-indent :inherit '(org-hide fixed-pitch))
  ;; (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch)) ; hide stars
  ;; (set-face-attribute 'org-link :foreground "royal blue" :underline t)
  ;; (set-face-attribute 'org-property-value :inherit fixed-pitch)
  )

;;; --- Org bullets: ----

(setup (:pkg org-bullets)
  (:hook-into org-mode)
  (:option org-bullets-bullet-list
	   '("⁖" "◉" "○" "✸" "✿")
	   ;; '("◉" "○" "●" "○" "●" "○" "●")
	   ))

;; ;; make org list dash into dot
;; (font-lock-add-keywords
;;  'org-mode
;;  '(("^ *\\([-]\\) "
;;     (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;;; --- Leader keys: ----

(leader-key-def
  "o"  '(:ignore t :which-key "org")
  "ox" '(eval-last-sexp :which-key "eval-last-sexp")
  "oX" '(eval-region :which-key "eval-region"))

;;; --- Org-babel: ----

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (scheme . t)
     (python . t)))
  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("scm" . "src scheme"))
  (add-to-list 'org-structure-template-alist '("unix" . "src conf-unix"))
  (add-to-list 'org-structure-template-alist '("conf" . "src conf"))
  (add-to-list 'org-structure-template-alist '("clang" . "src c"))
  (add-to-list 'org-structure-template-alist '("gcc" . "src c"))
  (add-to-list 'org-structure-template-alist '("cl" . "src common-lisp"))
  (add-to-list 'org-structure-template-alist
               '("mani" . "src scheme :noweb-ref packages :noweb-sep \"\"")))

(leader-key-def
  "ob"  '(:ignore t :which-key "org-babel")
  "obt" '(org-babel-tangle :which-key "tangle")
  "obe" '(org-babel-execute-src-block :which-key "org-babel-execute-src-block"))

;;; --- auto-tangle: ----

(setup (:pkg org-auto-tangle)
  (:hook-into org-mode))

;;; --- TOC: ----

(setup (:pkg toc-org)
  (:hook-into org-mode))

;;; --- Typographic symbols: ----

;; "C-c 8" to access
(setup (:pkg typo)
  (typo-global-mode 1))

;;; --- org-pomodoro: ----

(setup (:pkg org-pomodoro)
  (:global "C-c o P" org-pomodoro))

;;; --- Org-download: ----

;; TODO?
(setup (:pkg org-download))

;;; --- Org Hydra shortcuts: ----

(with-eval-after-load 'org
  ;; insert-mode when create new heading
  (defun ri/my--insert-heading-hook ()
    (if (eq meow-normal-mode t)
        (meow-insert-mode)))

  (setq org-insert-heading-hook '(ri/my--insert-heading-hook))

  ;; custom func for toggle heading
  (defun ri/my--toggle-heading ()
    (interactive)
    (org-back-to-heading)
    (org-cycle))

  ;; hydra for navigation
  (defhydra ri/hydra-org-navigation (:timeout 60)
    ;; heading navigation
    ("n" org-next-visible-heading "next")	     ; C-c C-n
    ("p" org-previous-visible-heading "prev")	     ; C-c C-p
    ("t" org-previous-visible-heading "prev")	     ; C-c C-p
    ;; 
    ("f" org-forward-heading-same-level "forward")   ; C-c C-f
    ("b" org-backward-heading-same-level "backward") ; C-c C-b
    ("s" org-forward-heading-same-level "forward")   ; C-c C-f
    ("h" org-backward-heading-same-level "backward") ; C-c C-b
    ;;
    ("u" outline-up-heading "up")	; C-c C-u
    ("d" outline-up-heading "up")	; C-c C-u
    ;; heading move
    ("H" org-metaleft "metaleft")	; <-
    ("T" org-metaup "metaup")		; ^^
    ("P" org-metaup "metaup")		; ^^
    ("N" org-metadown "metadown")	; v
    ("S" org-metaright "metaright")	; ->
    ;; page navigation
    ("j" ri/scroll-down-half-page "half down")
    ("k" ri/scroll-up-half-page "half up")
    ("/" ri/scroll-down-half-page "half down")
    ("?" ri/scroll-up-half-page "half up")
    ;;
    ("v" scroll-up-command "page up")
    ("V" scroll-down-command "page down")
    ;;
    ("," beginning-of-buffer "top of page")
    ("." end-of-buffer "end of page")
    ;; single down/up cursor
    ("C-n" next-line)
    ("C-p" previous-line)
    ("C-t" previous-line)
    ;; open/close
    ("TAB" ri/my--toggle-heading "open-close")
    ("c" org-shifttab "global-cycle")
    ;;
    ("g" nil "quit" :exit t))

  (leader-key-def
    "n" '(ri/hydra-org-navigation/body :which-key "hydra-navigation")))

(provide 'ri-org)
;;; ri-org.el ends here
