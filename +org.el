;;; ~/.doom.d/+org.el -*- lexical-binding: t; -*-

(add-hook 'org-mode-hook (lambda () (auto-fill-mode 0)))
(after! org
  (add-hook 'org-agenda-mode-hook (lambda () (visual-line-mode 0)))
  (require 'org-tempo)
  (setq org-duration-format (quote h:mm))
  (setq org-export-with-clocks t)
  (setq org-export-with-drawers t)
  (setq org-export-with-properties t)
  (setq org-agenda-sticky t)
  (setq org-adapt-indentation nil)
  (setq org-log-done t)
  ;; (setq org-list-allow-alphabetical t)
  (setq org-hide-leading-stars nil)
  (setq org-hide-leading-stars-before-indent-mode nil)
  (setq org-startup-indented nil)
  (setq org-startup-folded t)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  (setq org-src-window-setup 'current-window)
  (setq org-deadline-warning-days 7)
  (setq inbox-file (concat org-directory "inbox.org"))
  (setq todos-file (concat org-directory "inbox.org"))
  (setq life-file (concat org-directory "life.org"))
  (setq work-file (concat org-directory "work.org"))
  (setq someday-file (concat org-directory "someday.org"))
  (setq archive-file (concat org-directory "archive.org"))
  (setq brain-file "~/dev/notes/brain.org")
  (setq +org-capture-notes-file inbox-file)
  (setq +org-capture-todo-file todos-file)
  (setq org-default-notes-file inbox-file)
  (setq org-agenda-files (list inbox-file life-file work-file someday-file))
  (setq org-archive-location (concat archive-file "::* From %s"))
  (setq org-clock-rounding-minutes 5)

  (setq org-capture-templates
        '(("i" "Inbox" entry (file  inbox-file) "* TODO⚑ %?\nSCHEDULED: %^t \n %i\n")
          ("l" "Life" entry (file  life-file) "* TODO⚑ %?\nSCHEDULED: %^t \n %i\n")
          ("n" "Note" entry (file  brain-file) "* %?\n %i\n")
          ("s" "Someday" entry (file someday-file) "* TODO⚑ %?\n%i\n")
          ("t" "Todo" entry (file  inbox-file) "* TODO⚑ %?\nSCHEDULED: %^t \n %i\n")
          ("w" "Work" entry (file  work-file) "* TODO⚑ %?\nSCHEDULED: %^t \n %i\n")))

  (setq org-refile-targets '((org-agenda-files :level . 0)
                             (brain-file :maxlevel . 3)))

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


  (defun krofna-hack ()
    (when (looking-back (rx "$$ "))
      (save-excursion
        (backward-char 1)
        (org-toggle-latex-fragment))))

  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'post-self-insert-hook #'krofna-hack 'append 'local)))
  )

