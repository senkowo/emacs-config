


;; File management, recursively search for pattern in files

(leader-key-def
  ;; TODO: duplicate from ri-dired, create all prefixes first? actually idk...
  "d"  '(:ignore t :which-key "dired") 
  "dr" 'rgrep
  "dR" 'find-grep-dired)


;; hex color highlighting

;; (setup (:pkg rainbow-mode))

(use-package rainbow-mode)


















(provide 'ri-rest)
