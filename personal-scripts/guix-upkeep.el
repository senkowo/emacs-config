

;; Stole vterm scripts from here:
;; https://www.reddit.com/r/emacs/comments/ft84xy/run_shell_command_in_new_vterm/

(defun run-in-vterm-kill (process event)
  "A process sentinel. Kills PROCESS's buffer if it is live."
  (let ((b (process-buffer process)))
    (and (buffer-live-p b)
         (kill-buffer b))))

(defun run-in-vterm (command)
  "Execute string COMMAND in a new vterm.

Interactively, prompt for COMMAND with the current buffer's file
name supplied. When called from Dired, supply the name of the
file at point.

Like `async-shell-command`, but run in a vterm for full terminal features.

The new vterm buffer is named in the form `*foo bar.baz*`, the
command and its arguments in earmuffs.

When the command terminates, the shell remains open, but when the
shell exits, the buffer is killed."
  (interactive
   (list
    (let* ((f (cond (buffer-file-name)
                    ((eq major-mode 'dired-mode)
                     (dired-get-filename nil t))))
           (filename (concat " " (shell-quote-argument (and f (file-relative-name f))))))
      (read-shell-command "Terminal command: "
                          (cons filename 0)
                          (cons 'shell-command-history 1)
                          (list filename)))))
  (with-current-buffer (vterm (concat "*" command "*"))
    (set-process-sentinel vterm--process #'run-in-vterm-kill)
    (vterm-send-string command)
    (vterm-send-return)))





;;; Ideas:
;; - rewrite to make it super short, maybe use cl-loop?




(require 'cl-lib)

(defvar ri-cmd-updates
  '(("1.guix-channels" . "guix-update-channels")
    ("2.guix-profiles" . "guix-update-profiles all")
    ("3.guix-system"   . "guix-system-reconfigure 3")
    ("4.nix-packages"  . "nix-update-packages y")
    ("5.flatpak"       . "flatpak-update")
    ("6.arkenfox"      . "arkenfox-updater.sh")))

(defvar ri-cmd-backups
  "snapper backup")

(defvar ri-cmd-checks
  "guix gc --verify=contents")

(defvar ri-operation-types
  '(("updates" . ri-cmd-updates)
    ("backups" . ri-cmd-backups)
    ("checks"  . ri-cmd-checks)))

(defun ri/guix-upkeep ()
  (interactive)
  (let (options mode mode-val ret to-run)
    ;; prompt operation type
    (setq options (mapcar 'car ri-operation-types))
    (setq mode (cdr (assoc (completing-read "Select operation type:" options)
			   ri-operation-types)))
    ;; prompt nums/range of actions
    (setq mode-val (eval mode))
    (setq options (mapcar 'car mode-val))
    (let* ((opt-len (length options))
	   (opt-seq (number-sequence 1 opt-len))
	   (possible-inputs nil)
	   (inputs nil)
	   (selected-list (make-list opt-len nil)))
      ;; get all possible inputs
      (setq possible-inputs
	    (mapcan (lambda (i)
		      (append (list (format "%s" i)
				    (format "-%s" i)
				    "all" "exit" "quit")
			      (mapcan (lambda (j)
					(list (format "%s-%s" i j)
					      (format "-%s-%s" i j)))
				      (number-sequence (+ i 1) opt-len))))
		    opt-seq))
      ;; prompt for inputs
      (setq inputs (completing-read-multiple
		    (format "Select operation to run:\n%s"
			    (mapconcat (lambda (x)
					 (format "%s\n" x))
				       options)) 
		    possible-inputs nil 'require-match))
      
      ;; process inputs, set `selected-list' accordingly
      (mapcar (lambda (n)
		(cond ((string-match "^[0-9]+$" n)
		       (let ((idx (- (string-to-number n)
				     1)))
			 (setf (nth idx selected-list) 't)))
		      ((string-match "^-[0-9]+$" n)
		       (let ((idx (- (string-to-number
				      (substring n 1))
				     1)))
			 (setf (nth idx selected-list) 'nil)))
		      ((string-match "^[0-9]+-[0-9]+$" n)
		       (let ((beg (- (string-to-number
				      (replace-regexp-in-string "^\\([0-9]+\\).*$"
								"\\1" n))
				     1))
			     (end (- (string-to-number
				      (replace-regexp-in-string "^.*-\\([0-9]+\\)$"
								"\\1" n))
				     1)))
			 (mapc (lambda (i)
				 (setf (nth i selected-list) 't))
			       (number-sequence beg end))))
		      ((string-match "^-[0-9]+-[0-9]+$" n)
		       (let ((beg (- (string-to-number
				      (replace-regexp-in-string "^-\\([0-9]+\\).*$"
								"\\1" n))
				     1))
			     (end (- (string-to-number
				      (replace-regexp-in-string "^.*-\\([0-9]+\\)$"
								"\\1" n))
				     1)))
			 (mapc (lambda (i)
				 (setf (nth i selected-list) 'nil))
			       (number-sequence beg end))))))
	      inputs)

      ;; get ret (cdr) from alist using selected-list
      (dotimes (i opt-len)
	(when (nth i selected-list)
	  (setq ret (append ret (list (cdr (nth i mode-val))))))))

    ;; finally got list of commands to run!
    ;; (run-in-vterm)
    (setq to-run (concat (mapconcat (lambda (e)
				      (format "%s && " e))
				    ret)
			 "echo \"done! enter to exit.\" ; "
			 "read in ; "
			 "[ \"$in\" = \"\" ] && exit"))

    ;; run!
    (run-in-vterm to-run)))
