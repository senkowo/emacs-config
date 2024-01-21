;;; ri-theme --- adding some color to Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Install themes and set default theme.
;; Also load functions related to setting theme.

;;; Code:

;;; --- Themes: ----

(setup (:pkg doom-themes))
(setup (:pkg kaolin-themes))
(setup (:pkg ef-themes))

(defvar ri/theme nil
  "The current theme loaded.")
(setq ri/theme
      (car '(
	     ;; doom-moonlight ; feels like moonlight
	     ;; kaolin-eclipse ; excellent velvet
	     ef-trio-dark ; evening raspberry icecream
	     ;; ef-dark ; locked in dark
	     ;; kaolin-shiva ; soft pink velvet
	     )))
(load-theme ri/theme t)

;;; --- End: ----


(provide 'ri-theme)
;;; ri-theme.el ends here
