;;; ~/.doom.d/trash.el -*- lexical-binding: t; -*-


;; (remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
;; (remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)

;; (defun recent-file-list ()
;; (seq-take recentf-list 5))

;; (add-hook '+doom-dashboard-functions #'recent-file-list)

;; (defun lolfunc ()
;;   (require 'seq)
;;   (setq lelist (seq-take recentf-list 5))
;;   (insert
;;    "\n"
;;    (+doom-dashboard--center
;;     (- +doom-dashboard--width 2)
;;     (with-temp-buffer
;;       ;; (seq-map #'insert lelist)
;;       (insert "hiii")
;;       ;; (insert-text-button (propertize "github" 'face 'doom-dashboard-footer)
;;       ;;                     'action (lambda (_) (browse-url "https://github.com/hlissner/doom-emacs"))
;;       ;;                     'follow-link t
;;       ;;                     'help-echo "Open Doom Emacs github page")
;;       (buffer-string)
;;       ))
;;    "\n"))

;; (add-hook '+doom-dashboard-functions #'lolfunc)



;; ;; (length lelist)
