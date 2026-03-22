(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("350fef8767e45b0f81dd54c986ee6854857f27067bac88d2b1c2a6fa7fecb522" default))
 '(ignored-local-variable-values '((eval progn (pp-buffer) (indent-buffer))))
 '(safe-local-variable-values
   '((eval progn
      (setq-local gac-commit-message-function
       (lambda (filename)
         (format "%s %s" (format-time-string "%Y-%m-%d %H:%M")
                 (file-name-nondirectory filename))))
      (git-auto-commit-mode 1))
     (gac-automatically-add-new-files-p) (gac-automatically-push-p)
     (eval run-at-time 0 nil
      (lambda nil
        (when (string-match "dashboard.org" (buffer-name))
          (goto-char (point-max)) (org-reveal))))
     (eval progn (org-show-entry) (goto-char (point-max)) (org-show-entry)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
