;;; ri-package --- setup straight.el -*- lexical-binding: t; mode: emacs-lisp; mode: outline-minor; -*-

;;; Commentary:
;;  Set up straight.el, the package manager Emacs uses.

;;; Code:

;;; --- Straight ----

;; [[https://github.crookster.org/switching-to-straight.el-from-emacs-26-builtin-package.el/][Blog - Switching from use-package to straight.el]]
;; use 'straight-remove-unused-repos' to remove unused packages
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; disable compiler warnings when compiling packages.
(setq native-comp-async-report-warnings-errors nil)
;; TODO: necessary? directory from which to search for compiled packages
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

;; install use-package
(straight-use-package 'use-package)
;; no need to put :straight with use-package
(setq straight-use-package-by-default t)

;; TODO: what does this do?
;; Load the helper package for commands like `straight-x-clean-unused-repos'
;; Also use `straight-pull-all'
(require 'straight-x)

;;; --- debugging ----

(defvar ri/debug-straight nil
  "Show debug ouptut for straight.el after loading a file.")

(defun dw/log-require (&rest args)
  "Add to list 'after-load-functions to debug straight.el.
Not sure what ARGS does."
  (with-current-buffer (get-buffer-create "*require-log*")
    (insert (format "%s\n"
                    (file-name-nondirectory (car args))))))
(when ri/debug-straight
  (add-to-list 'after-load-functions #'dw/log-require))


(provide 'ri-package)
;;; ri-package.el ends here
