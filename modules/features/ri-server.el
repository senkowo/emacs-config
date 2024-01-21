;;; ri-server --- enable Emacs server -*- lexical-binding: t; -*-

;;; Commentary:
;; Start Emacs server if not already started.
;; Emacs server makes it so you can start Emacs clients for the same
;; Emacs session by running 'emacsclient' in a terminal.

;;; Code:

;;; --- Server: ----
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
	    (unless (or (boundp 'server-process)
			(processp server-process)
			(server-running-p))
	      (server-start)
	      (message "Emacsclient Server started!"))))


(provide 'ri-server)
;;; ri-server.el ends here
