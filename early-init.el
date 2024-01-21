;;; early-init.el --- run before startup -*- lexical-binding: t; -*-

;;; Commentary:
;; Prevent package.el from loading

;;; Code:

;; For straight.el (prevent package.el from loading)
(setq package-enable-at-startup nil)


;;; early-init.el ends here
