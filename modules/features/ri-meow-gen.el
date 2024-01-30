;;; ri-meow-gen --- Meow modal keys - General settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Sets up Meow (modal editor like evil-mode).
;;
;; This file sets general Meow settings that work for all keyboard layouts.
;; This file will need to be accompanied by a file that loads
;; keyboard-specific keybinds for Meow.
;; (for example, ri-meow-dvp (dvorak-programmer) or ri-meow-qwerty (WIP)).
;;
;; Notes:
;; - maybe make C-M-g exit everything reguardless of state?

;;; Code:

(require 'setup)
(require 'cl-extra)

;;; --- Functions and variables relating to Meow: ----

(defun ri/meow-exit-all-and-save ()
  "When run, exit meow insert mode, exit snippet, then save buffer."
  (interactive)
  (meow-insert-exit)
  (yas-abort-snippet)
  (save-buffer)
  (keyboard-quit)			; TODO: this necessary?
  )

(defvar ri/meow-insert-default-modes
  '(vterm-mode
    eshell-mode)
  "Start these modes in meow-insert-mode.")

(defvar ri/meow-SPC-ignore-list
  '(Info-mode
    gnus-summary-mode
    gnus-article-mode
    w3m-mode)
  "Disable meow-keypad in these modes.")

;;; --- Meow: ----

;; useful notes:
;; - meow-keypad-start-keys: SPC-c -> C-c , etc...
;; - (meow-define-state): create meow mode/state for <C-c n> or anything.

(setup (:pkg meow)
  (require 'meow)
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-dvp)
  (setq meow-use-cursor-position-hack t) ; TODO: insert/append work without?
  (require 'which-key)	     ; which-key must be init module, assuming
  (setq meow-keypad-describe-delay which-key-idle-delay) ; delay before popup

  ;; set some default keybinds
  (meow-define-keys 'insert
    '("C-g" . meow-insert-exit)
    '("C-M-g" . ri/meow-exit-all-and-save))
  (:global "C-c c" ri/meow-exit-all-and-save)

  ;; start certain modes in insert-mode
  (dolist (mode ri/meow-insert-default-modes)
    (add-to-list 'meow-mode-state-list `(,mode . insert)))

  ;; disable meow-keypad (space key) on certain modes
  (defun ri/meow-SPC-ignore ()
    "When run, either run SPC for the current major-mode or meow-keypad."
    (interactive)
    ;; if t, appropriate to replace and run mode cmd.
    (if-let ((in-ignore-list?
	      (cl-some (lambda (mode)
			 (eq major-mode mode))
		       ri/meow-SPC-ignore-list))
	     (cmd-to-replace
	      (lookup-key (current-local-map) (kbd "SPC"))))
	(funcall cmd-to-replace)
      (meow-keypad)))

  (meow-motion-overwrite-define-key
   '("SPC" . ri/meow-SPC-ignore)))

;;; --- Check at Emacs startup if a keyboard layout was configured: ----

(defvar ri/meow-layout-configured? nil
  "If nil after Emacs loads, Meow was loaded but without a keyboard layout.")

(add-hook 'emacs-startup-hook ; TODO: make into add-to-list? duplicates?
	  (lambda ()
	    (unless ri/meow-layout-configured?
	      (message "NOTE: meow loaded w/o a keyboard layout config.
      If you did, then toggle the variable `ri/meow-layout-configured?'"))))

;;; --- Meow custom keybinds: ----

(defun ri/meow-delete-or-kill ()
  "If no region is selected, `meow-delete', otherwise `meow-kill'."
  (interactive)
  (let ((select-enable-clipboard meow-use-clipboard))
    (when (meow--allow-modify-p)
      (meow--with-selection-fallback
       (cond
	((not (meow--selection-type))
	 ;; basically meow-C-d
	 (meow--execute-kbd-macro meow--kbd-delete-char))
	(t
	 ;; kills region
	 (meow--prepare-region-for-kill)
         (meow--execute-kbd-macro meow--kbd-kill-region)))))))


(provide 'ri-meow-gen)
;;; ri-meow-gen.el ends here
