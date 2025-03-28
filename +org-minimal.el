;;; +org-minimal.el -*- lexical-binding: t; -*-

(after! org
  (setq org-startup-folded t)
  (setq agenda-file (concat org-directory "agenda.org"))
  (setq archive-file (concat org-directory "archive.org"))

  (setq org-agenda-files (list agenda-file))
  (setq org-archive-location (concat archive-file "::* From %s"))

  (setq org-capture-templates
        '(
          ("a" "auto")
          ("aj" "Auto Journal" plain (file+olp+datetree journal-file) (file "~/library/org-brain/dailyplan-template.txt") :immediate-finish t :jump-to-captured t)
          ("j" "Agenda" entry (file+olp+datetree agenda-file) "* %?\n%i\n" :jump-to-captured t)))


  (setq org-todo-keywords (quote((sequence "TODO⚑" "SOMEDAY⚐" "IN-PROGRESS/WAITING⚐" "|" "DONE✔" "CANCELED✘"))))
  (setq org-todo-keyword-faces
        '(("TODO⚑" . "deep sky blue")
          ("SOMEDAY⚐" . "cornflower blue")
          ("IN-PROGRESS/WAITING⚐" . "orange")
          ("DONE✔" . "medium spring green")
          ("CANCELED✘" . (:foreground "red"))))
  )
