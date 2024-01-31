;;; ri-def-fonts --- set default fonts -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
;; Set default fonts.
;; TODO: add fonts to local dir and load from it.

;;; --- Fonts: ----

;; default font (modeline, minibuffer, default for applications, etc)
(set-face-attribute 'default nil :font "Fira Code" :height 110 :foreground "white")
;; (set-face-attribute 'default nil :font "JetBrains Mono" :height 115)

;; fixed pitch font (code blocks, property, startup, etc (can add more))
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height 110)

;; variable pitch font (toc links, regular text in org, etc...)
;; how about Iosveka instead?
;; TODO: move this to org mode?
;; (set-face-attribute 'variable-pitch nil :font "DejaVu Sans" :height 125 :weight 'regular)


(provide 'ri-def-fonts)
;;; ri-def-fonts.el ends here
