;;; ~/.doom.d/+org.el -*- lexical-binding: t; -*-
(load-file "~/.doom.d/+misc/beautiful-org.el")

(add-hook 'org-mode-hook (lambda () (auto-fill-mode 0)))

(after! org
  (add-hook 'org-agenda-mode-hook (lambda () (visual-line-mode 0)))
  (require 'org-tempo)
  ;; (add-to-list 'org-modules 'org-habit)
  (setq org-duration-format (quote h:mm))

  ;; (setq org-ellipsis " ▼ ") ;; folding symbol
  ;; (setq org-ellipsis "  ")  ;; folding symbol
  (setq org-log-into-drawer t)
  (setq org-ellipsis " ▼ ")  ;; folding symbol
  (setq org-cycle-separator-lines -1) ;; to not consider blank lines as part of heading and resolve display issue with ellipsis char and []
  (setq org-export-with-clocks t)
  (setq org-export-with-drawers t)
  (setq org-export-with-properties t)
  (setq org-agenda-sticky t)
  ;; new for nice writing & note taking experience
  (setq org-adapt-indentation nil) ;; if content below org header should align / get indented
  (setq org-hide-leading-stars nil)
  (setq org-startup-indented nil)
  ;; (setq org-startup-indented t)
  (setq org-log-done t)
  ;; (setq org-list-allow-alphabetical t)
  (setq org-hide-leading-stars-before-indent-mode nil)
  (setq org-startup-folded t)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  (setq org-src-window-setup 'current-window)
  (setq org-deadline-warning-days 7)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; For quickly capturing things that will be filtered & planned later ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq inbox-file (concat org-directory "inbox.org"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; For daily journaling ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq journal-file (concat org-directory "journal.org"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; For things that don't have a date and ideas  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq someday-file (concat org-directory "someday.org"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; For scheduled/planned things in the future ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq todo-file (concat org-directory "todo.org"))


  (setq archive-file (concat org-directory "archive.org"))
  (setq dailyplan-template (concat org-directory "dailyplan-template.txt"))

  (setq +org-capture-notes-file inbox-file)
  (setq +org-capture-todo-file todo-file)
  (setq org-default-notes-file inbox-file)
  (setq org-agenda-files (list inbox-file journal-file someday-file todo-file))
  (setq org-archive-location (concat archive-file "::* From %s"))
  (setq org-clock-rounding-minutes 5)

  (setq org-capture-templates
        '(
          ("a" "auto")
          ("aj" "Auto Journal" plain (file+olp+datetree journal-file) (file "~/library/org-brain/dailyplan-template.txt") :immediate-finish t :jump-to-captured t)

          ("i" "Inbox" entry (file  inbox-file) "* TODO⚑ %?\nSCHEDULED: %^t \n %i\n")
          ("j" "Journal" entry (file+olp+datetree journal-file) "* %?\n%i\n" :jump-to-captured t)
          ("s" "Someday" entry (file someday-file) "* TODO⚑ %?\n%i\n")
          ("t" "Todo" entry (file  todo-file) "* TODO⚑ %?\nSCHEDULED: %^t \n %i\n")))

  (setq org-refile-targets '((org-agenda-files :level . 0)))

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

  (setq org-todo-keywords (quote((sequence "TODO⚑" "SOMEDAY⚐" "IN-PROGRESS/WAITING⚐" "|" "DONE✔" "CANCELED✘"))))
  (setq org-todo-keyword-faces
        '(("TODO⚑" . "deep sky blue")
          ("SOMEDAY⚐" . "cornflower blue")
          ("IN-PROGRESS/WAITING⚐" . "orange")
          ("DONE✔" . "medium spring green")
          ("CANCELED✘" . (:foreground "red"))))


  ;; (defun krofna-hack ()
  ;;   (when (looking-back (rx "$$ "))
  ;;     (save-excursion
  ;;       (backward-char 1)
  ;;       (org-toggle-latex-fragment))))

(setq org-agenda-scheduled-leaders '("" ""))
  (setq org-agenda-prefix-format '(
  (agenda  . " %i %-12:t% s ") ;; file name + org-agenda-entry-type
  ;; (agenda  . "  • ")
  (timeline  . "  % s")
  (todo  . " %i %-12:c")
  (tags  . " %i %-12:c")
  (search . " %i %-12:c")))

  ;; (add-hook 'org-mode-hook
  ;;           (lambda ()
  ;;             (add-hook 'post-self-insert-hook #'krofna-hack 'append 'local)))
  (add-hook 'org-mode-hook
      (lambda ()
        (add-hook 'after-save-hook 'org-preview-latex-fragment nil 'make-it-local))
      (when (string-match-p (regexp-quote ".http.org") (buffer-name))
        (verb-mode 1)
        (map! :map org-mode-map
              :nv "C-c C-c" #'verb-send-request-on-point-other-window
              )
        )
      )
  )

;; For citations
(setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory %o %f"
        "biber %b"
        "pdflatex -interaction nonstopmode -output-directory %o %f"
        "pdflatex -interaction nonstopmode -output-directory %o %f"))
