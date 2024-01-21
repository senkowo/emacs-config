;;; ri-basic-ui --- tweaks to default UI -*- lexical-binding: t; -*-

;;; Commentary:
;; Adjusts the default Emacs UI so that it's usable and good.
;; Configures:
;; - unclutters UI
;; - enables small quality of life functionality
;;  - recentf and save location in file
;; - disables bell
;; - line numbers
;; - scrolling
;; - shorten y-or-n prompt

;;; Code:


;;; --- Basic tweaks: ----

;; disable startup screen
(setq inhibit-startup-message nil) ; TODO: auto-becomes t w/ dashboard?

;; disable ui
(scroll-bar-mode -1) ; disable visible scrollbar
(tool-bar-mode -1)   ; disable the toolbar
(tooltip-mode 1)     ; disable tooltips
(set-fringe-mode 10) ; give some breathing room
(menu-bar-mode -1)   ; disable menu bar

;; other ; TODO: enable the two commented?
;; TODO: move this into a better place?
(setq fill-column 70) ; shadowed by visual-fill-column if visual-fill-column-mode is non-nil.
(recentf-mode 1) ; show recent files when viewing files (counsel enables by def).
(save-place-mode 1) ; go to previous location in file when reopening.

;; disable bell
(setq ring-bell-function 'ignore)

;;; --- Scrolling: ----

;; improve scrolling functionality (when cursor at bottom, screen will
;; move along a few lines, then then jump forward half a page).
;; TODO: try to find more natural way of scrolling.
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))) ; scroll when using mouse wheel.
(setq mouse-wheel-progressive-speed nil) ; don't accelerate scrolling.
(setq mouse-wheel-follow-mouse 't) ; scroll window under mouse regardless of focus.
(setq scroll-conservatively 0) ; move window when moving off screen.
(setq scroll-margin 0) ; margin before scroll at top and bottom of screen.
(setq scroll-step 1) ; keyboard scroll one line at a time.
(setq use-dialog-box nil) ; (change to nil) make things like yes or no prompts dialogue boxes.

;;; --- Shorten y-n prompt: ----

(defalias 'yes-or-no-p 'y-or-n-p)

;;; --- End: ----


(provide 'ri-basic-ui)
;;; ri-basic-ui.el ends here
