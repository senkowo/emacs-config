;;; ri-completion-ivy.el --- the ivy completion system -*- lexical-binding: t; -*-

;;; Commentary:
;; TODO: can some of the ivy fuck up code completion and stuff like that???
;; So is it better to sometimes disable Ivy-related stuff for IDE n stuff?
;; https://oremacs.com/swiper/#key-bindings ; ivy docs maybe?

;;; Code:

(require 'setup)

;;; --- Ivy: ----

;; TODO: improve these default keys!!
;;  Add more removed keybinds!!!
;;  Add some for evil!!!

(setup (:pkg ivy)
  (diminish 'ivy)
  ;; (:also-load ivy-rich) ; TODO: have this INSIDE ivy-rich??
  (:global "C-s" swiper ;; fuzzy search tool
           "C-c s p" swiper-isearch-thing-at-point)
  (:with-map ivy-minibuffer-map
    (:bind "TAB"   ivy-partial-or-done
	   "C-M-d" ivy-immediate-done
	   ;; Evil mode: ; TODO: FIX!!!
	   ))
  ;; Configure: --
  (setq ivy-height 15)
  ;; TODO: what is this???!!!!
  ;; fixes bug with swiper breaking when hovering over links.
  ;; After the bug has been fixed, change it back to `text-properties'.
  ;; (setq org-fold-core-style 'overlays) ; 'text-properties (def) is faster
  ;; (setq org-fold-core-style 'text-properties) ; 'text-properties (def) is faster
  (ivy-mode 1))

;;; --- Counsel: ----

;; counsel (ivy-enhanced standard Emacs commands)

(setup (:pkg counsel)
  (:global "C-x b" counsel-switch-buffer) ; TODO: do i really need this?
  (:with-map minibuffer-local-map
    (:bind "C-r" counsel-minibuffer-history))
  ;; Config
  ;; TODO: wtf is this...
  (setq counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  (setq ivy-initial-inputs-alist nil)	; dont start searches with ^
  (counsel-mode 1))

;;; --- Ivy rich: ----

(setup (:pkg ivy-rich)
  (:load-after ivy)
  (ivy-rich-mode 1))

;;; --- Ivy Prescient: ---

;; adds ivy completion regex and order commands by last used

(setup (:pkg ivy-prescient)
  (:load-after counsel)
  (setq ivy-prescient-enable-filtering nil)
  ;; remember sorting across sessions!
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))



(provide 'ri-completion-ivy)
;;; ri-completion-ivy.el ends here
