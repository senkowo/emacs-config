;;; ri-dirvish --- a fancier dired -*- lexical-binding: t; -*-

;;; Commentary:

;; Dirvish depends on:
;; - fd
;; - imagemagik for image preview
;; - poppler | pdf-tools for pdf preview
;; - ffmpegthumbnailer for video preview
;; - mediainfo for audio/video metadata generation
;; - tar and unzip for archive files preview

;;; Code:

(require 'setup)
(require 'ri-dired) ; load my dired module

(setup (:pkg dirvish)
  ;; Make all new dired buffers open in dirvish.
  (dirvish-override-dired-mode 1)
  ;; quick jump to directories
  (setq dirvish-quick-access-entries ; It's a custom option, `setq' won't work
	'(("h" "~/"                          "Home")
	  ("o" "~/Notes/org/"                "Org")
	  ("s" "~/Notes/School/"             "School")
	  ("c" "~/Notes/School/classes/current/"	"classes")
	  ("C" "~/Code/"                     "Code")
	  ("d" "~/Downloads/"                "Downloads")
	  ("p" "~/Pictures/"                 "Pictures")
	  ("e" "~/.dotfiles/.emacs.d/"       "Emacs user directory")
	  ("g" "~/yui/guix"                  "Guix config")
	  ("y" "~/yui/channel"               "Yumi channel")
	  ("N" "~/.newmacs.d"                "Newmacs config")
	  ("m" "/mnt/"                       "Drives")
	  ("t" "~/.local/share/Trash/files/" "Trash")))
  ;; (define-key dirvish-mode-map (kbd "z") 'ri/dired-hide-dotfiles-mode--toggle)
  ;; How to display files in directory:
  (setq dired-listing-switches
        ;; "-ahgo --group-directories-first"
        "-l --almost-all --human-readable --group-directories-first --no-group") ; AhoG
  ;; TODO: what's this?
  ;; (setq dired-dwim-target t) ; auto select dir to move to if another dired window open.
  (setq delete-by-moving-to-trash t) ; NOTE: depends on trash-cli
  ;; Show file previews
  (dirvish-peek-mode 1)
  ;; File attributes to show
  (setq dirvish-attributes
        '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg))
  ;; Configure how to preview
  (setq dirvish-preview-dispatchers
	;; TODO: this shows only the first page of a pdf!
        (cl-substitute 'pdf-preface 'pdf dirvish-preview-dispatchers))
  ;; External programs to open files with
  ;; TODO: what's the difference between this and the dired version???
  (setq dirvish-open-with-programs
        (let ((mpv (or (executable-find "mpv") "mpv")))
          `((,dirvish-audio-exts . (,mpv "--profile=builtin-pseudo-gui" "%f"))
            (,dirvish-video-exts . (,mpv "%f")))))
  (:global "C-c d D" dirvish
	   "C-c d f" dirvish-fd
	   "C-c D" dirvish-quick-access
	   "C-c d a" dirvish-quick-access)
  ;; Set dirvish interface keybinds:
  ;; (note: irvish inherits `dired-mode-map')
  (:with-map dirvish-mode-map
    (:bind
     "h" dired-up-directory
     "r" dired-sort-toggle-or-edit
     "s" dired-open-file
     "'" ri/dired-hide-dotfiles-hack-toggle
     ";" dirvish
     "N" dired-create-empty-file
     "M-T" ri/dired-set-wallpaper
     "/" dired-isearch-filenames-regexp
     "a" dirvish-quick-access)))

;; neat little function i made for dirvish
(defun ri/dired-set-wallpaper ()
  "Run \"feh --bg-fill\" on current file in Dirvish."
  (interactive)
  (let ((path (dired-get-filename)))
    (start-process-shell-command
     "feh dired" nil
     (concat "feh --bg-fill \"" path "\""))))


(provide 'ri-dirvish)
;;; ri-dirvish.el ends here
