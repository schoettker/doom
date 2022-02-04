;;; ~/.doom.d/+functions.el -*- lexical-binding: t; -*-

(defun add-semicolon ()
  (interactive)
  (end-of-line)
  (when (not (looking-back ";"))
    (insert ";"))
  (evil-first-non-blank))

(defun open-terminal ()
  (interactive "@")
  (shell-command (concat "alacritty"
                         " > /dev/null 2>&1 & disown") nil nil))

(defun my-change-number-at-point (change)
  (let ((number (number-at-point))
        (point (point)))
    (when number
      (progn
        (forward-word)
        (search-backward (number-to-string number))
        (replace-match (number-to-string (funcall change number)))
        (goto-char point)))))

(defun my-increment-number-at-point ()
  "Increment number at point like vim's C-a"
  (interactive)
  (my-change-number-at-point '1+))

(defun my-decrement-number-at-point ()
  "Decrement number at point like vim's C-x"
  (interactive)
  (my-change-number-at-point '1-))

(defun create-buffer(name)
  (interactive "sEnter name for new buffer: ")
  (switch-to-buffer
   (find-file (concat "/tmp/" name))))


(defun +emms-start-or-playlist ()
  (interactive)
  (if (fboundp 'emms) (emms)
    (call-interactively #'emms-add-directory)
    (emms)))

(add-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode)


;; Automatically clear transaction after adding
(defadvice ledger-add-transaction (after ledger-add-transaction activate) ( ledger-toggle-current ))

(defun open-books-library ()
  (interactive)
  (dired "~/library/books"))

(defun open-library ()
  (interactive)
  (dired "~/library"))

(add-hook 'c++-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (concat "g++ " buffer-file-name " && ./a.out"  ))
            (flycheck-mode -1)
            ))
;; (map! :map c++-mode-map
;;       :nvime "<f3>" #'compile
;;       )

;; (add-hook 'c++-mode-hook
;;       (lambda ()
;;         (set (make-local-variable 'compile-command)
;;              (format "g++ -g %s -o %s" (buffer-name) (file-name-sans-extension (buffer-name))))))

(add-hook 'ess-mode-hook
          (lambda ()
            (outline-minor-mode)
            (setq-local outline-regexp "## \\* ")
            ))

(defun custom-dashboard-insert-recents (list-size)
  "Add the list of LIST-SIZE items from recently edited files."
  (setq dashboard--recentf-cache-item-format nil)
  (recentf-mode)
  (let ((inhibit-message t) (message-log-max nil)) (recentf-cleanup))
  (dashboard-insert-section
   "Recent Files:"
   (dashboard-shorten-paths recentf-list 'dashboard-recentf-alist 'recents)
   list-size
   (dashboard-get-shortcut 'recents)
   `(lambda (&rest ignore)
      (find-file-existing (dashboard-expand-path-alist ,el dashboard-recentf-alist)))
   (let* ((file (dashboard-expand-path-alist el dashboard-recentf-alist))
          (filename (dashboard-f-filename file))
          (path (concat "["(concat (substring el 0 1) (concat "] " (dashboard-extract-key-path-alist el dashboard-recentf-alist))))))
     (cond
      ((eq dashboard-recentf-show-base 'align)
       (unless dashboard--recentf-cache-item-format
         (let* ((len-align (dashboard--align-length-by-type 'recents))
                (new-fmt (dashboard--generate-align-format
                          dashboard-recentf-item-format len-align)))
           (setq dashboard--recentf-cache-item-format new-fmt)))
       (format dashboard--recentf-cache-item-format filename path))
      ((null dashboard-recentf-show-base) path)
      (t (format dashboard-recentf-item-format filename path))))))
