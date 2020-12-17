;;; ~/.doom.d/+functions.el -*- lexical-binding: t; -*-

(defun add-semicolon ()
  (interactive)
  (end-of-line)
  (when (not (looking-back ";"))
    (insert ";"))
  (evil-first-non-blank))

(defun open-termite ()
  (interactive "@")
  (shell-command (concat "termite"
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

