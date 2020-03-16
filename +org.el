;;; ~/.doom.d/+org.el -*- lexical-binding: t; -*-

(add-hook 'org-mode-hook (lambda () (auto-fill-mode 0)))
(after! org
  (add-hook 'org-agenda-mode-hook (lambda () (visual-line-mode 0)))
  (setq org-agenda-sticky t)
  (setq org-adapt-indentation nil)
  (setq org-log-done t)
  (setq org-hide-leading-stars nil)
  (setq org-hide-leading-stars-before-indent-mode nil)
  (setq org-startup-indented nil)

  (defun export-to-html-light ()
    (interactive)
    (if (not (boundp 'sakura-light))
        (load-file "~/.doom.d/+misc/sakura-light.el")
      (setq org-html-head sakura-light))
    (org-html-export-to-html))

  (defun export-to-html-dark ()
    (interactive)
    (if (not (boundp 'sakura-dark))
        (load-file "~/.doom.d/+misc/sakura-dark.el")
      (setq org-html-head sakura-dark))
    (org-html-export-to-html))

  (setq org-todo-keywords (quote((sequence "TODO⚑" "IN-PROGRESS/WAITING⚐" "|" "DONE✔" "CANCELED✘"))))
  (setq org-todo-keyword-faces
        '(("TODO⚑" . "deep sky blue")
          ("IN-PROGRESS/WAITING⚐" . "orange")
          ("DONE✔" . "medium spring green")
          ("CANCELED✘" . (:foreground "red"))))


  (map! :map org-mode-map
        :localleader
        :desc "toggle checkbox" :n "c" #'org-toggle-checkbox
        :desc "todo" :n "t" #'org-insert-todo-heading
        :desc "link" :n "l" #'org-insert-link
        :desc "narrow" :n "n" #'org-narrow-to-subtree
        ))
