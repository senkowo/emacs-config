;;; ri-guix --- guix stuff -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'setup)

;;; --- guix package: ----

(setup (:pkg guix)
  (leader-key-def
    "G"  '(:ignore t :which-key "Guix")
    "Gg" '(guix :which-key "Guix")
    "Gp" '(guix-packages-by-name :which-key "search packages")))


(provide 'ri-guix)
;;; ri-guix.el ends here
