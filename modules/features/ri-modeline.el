;;; ri-modeline --- modeline config -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'setup)

;;; --- Smart-modeline: ----

;; NOTE: breaks shit

;; (setup (:pkg smart-mode-line)
;;   (setq sml/no-confirm-load-theme t)
;;   (setq sml/theme 'automatic)
;;   (smart-mode-line-enable 1))

;;; --- Mini-modeline: ----

;; flashing when scrolling to the side

;; (setup (:pkg mini-modeline)
;;   (mini-modeline-mode 1))

;;; --- Mood-line: ----

;; TODO: increase size by just a little.
(setup (:pkg mood-line)
  ;; initialize before config
  (mood-line-mode 1)

  ;; prettify symbols?
  ;; (setq mood-line-glyph-alist mood-line-glyphs-ascii)
  (setq mood-line-glyph-alist mood-line-glyphs-fira-code) ; TODO: depends on fira-code
  ;; (setq mood-line-glyph-alist mood-line-glyphs-unicode)

  ;; format? amount of detail?
  ;; (setq mood-line-format mood-line-format-default-extended)
  (setq mood-line-format mood-line-format-default))

;;; --- Doom-modeline: ----

;; (setup (:pkg doom-modeline)
;;   (require 'doom-modeline)
;;   ;; :init (doom-modeline-mode 1)
;;   ;; (:hook-into after-init-hook)
;;   ;; Hacky bugfix with modeline
;;   (display-battery-mode nil)
;;   (display-time-mode nil)
;;   (display-battery-mode t)
;;   (display-time-mode t)
;;   (doom-modeline-mode 1)
;;   (:option doom-dracula-brighter-modeline nil
;; 	   doom-modeline-height 32 ; 45? 40? best 20, stuck with 30 for a while
;; 	   doom-modeline-hud nil))


(provide 'ri-modeline)
;;; ri-modeline.el ends here
