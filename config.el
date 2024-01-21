;;; config.el --- a simple interface to configure Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Here, you can define which modules or packages to load.
;; This file will be read before loading any modules, so after setting
;; some values either restart Emacs or reload the associated module.
;;
;; Notes:
;; - use setup macros to choose not to load if not enabled here.
;;

;;; Code:

(defvar rf/config-to-enable nil)
(setq rf/config-to-enable
      '(
	ri-package
	ri-setup
	ri-pivotal
	ri-theme
	ri-core
	ri-org
	ri-server
	)

      )



;;; config.el ends here
