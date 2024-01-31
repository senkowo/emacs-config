;;; ri-pivotal --- pivotal things to load -*- lexical-binding: t; mode: emacs-lisp; mode: outline-minor; -*-

;;; Commentary:
;; Load this after loading ri-package and ri-setup.
;; This will configure:
;; - no-littering
;;

;;; Code:

;;; --- Prevent Littering: ----

(setup (:pkg no-littering)
  (require 'no-littering)
  ;; keep temporary customizations until Emacs closes
  (setq custom-file (make-temp-file "emacs-custom"))

  ;; Don't litter project folders with backup files
  (let ((backup-dir (no-littering-expand-var-file-name "backup/")))
    (make-directory backup-dir t)
    (setq backup-directory-alist
          `(("\\`/tmp/" . nil)
            ("\\`/dev/shm/" . nil)
            ("." . ,backup-dir))))

  ;; Tidy up auto-save files
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  ;; (let ((auto-save-dir (no-littering-expand-var-file-name "auto-save/")))
  ;;   (make-directory auto-save-dir t)
  ;;   (setq auto-save-file-name-transforms
  ;;         `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
  ;;            ,(concat temporary-file-directory "\\2") t)
  ;;           ("\\`\\(/tmp\\|/dev/shm\\)\\([^/]*/\\)*\\(.*\\)\\'" "\\3")
  ;;           ("." ,auto-save-dir t))))
  )

;;; --- Diminish: ----

;; TODO: move elsewhere, make my own function that loads the
;; module from inside here!!! Yoooo!!!

(setup (:pkg diminish))

;;; --- End: ----

(provide 'ri-pivotal)
;;; ri-pivotal.el ends here
