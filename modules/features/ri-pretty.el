;;; ri-pretty --- pretty stuff -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'setup)

;;; --- Dashboard: ----

(setup (:pkg dashboard)
  (require 'dashboard)
  ;; Do this manually instead.
  ;; (dashboard-setup-startup-hook)
  
  ;; Make n/p do j/k
  (:bind "n" dashboard-next-line
	 "p" dashboard-previous-line)
  
  (add-hook 'after-init-hook (lambda ()
  			       ;; Display useful lists of items
  			       (dashboard-insert-startupify-lists)))
  (add-hook 'emacs-startup-hook (lambda ()
                                  (switch-to-buffer dashboard-buffer-name)
                                  (goto-char (point-min))
                                  (redisplay)
                                  (run-hooks 'dashboard-after-initialize-hook)))
  ;; show dashboard on emacsclient creation
  ;; (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

  ;; logo/banner
  ;; (setq dashboard-startup-banner "/home/yui/Pictures/screenshots/hpcc_logo3_ALPHA_1000_c1.png")
  ;; (setq dashboard-startup-banner "/home/yui/Pictures/screenshots/hackucf2_alpha1_200.png")
  ;; (setq dashboard-startup-banner "/home/yui/Pictures/screenshots/hpcc_logo3_ALPHA_1000_c2.png")
  (setq dashboard-startup-banner (concat user-emacs-directory "misc/icons/horse3-1000.png"))
  ;; dont actually do this, edit the actual image to fix the size instead.
  ;; (setq dashboard-image-banner-max-height 200)
  )


(provide 'ri-pretty)
;;; ri-pretty.el ends here
