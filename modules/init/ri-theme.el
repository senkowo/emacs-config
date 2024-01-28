;;; ri-theme --- adding some color to Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Install themes and set default theme.
;; Also load functions related to setting theme.

;;; Code:

(require 'setup)

;;; --- Themes: ----

(setq custom-theme-directory (concat user-emacs-directory "misc/themes"))

(setup (:pkg doom-themes)
  ;; TODO: what does this do? does it have any real effect? read docs...
  ;; (doom-themes-org-config) ; TODO: note: SLOW. after?
  ;; TODO: none of the doom themes aren't loading...
  )
(setup (:pkg kaolin-themes))
(setup (:pkg ef-themes))

(defvar ri/theme nil
  "The current theme loaded.")
(setq ri/theme
      ;; 'doom-moonlight ; feels like moonlight
      ;; 'kaolin-eclipse ; excellent velvet
      ;; 'ef-trio-dark ; evening raspberry icecream
      ;; 'ef-dark ; locked in dark
      'kaolin-shiva ; soft pink blossom
      )
(load-theme ri/theme t)

;;; --- End: ----


(provide 'ri-theme)
;;; ri-theme.el ends here
