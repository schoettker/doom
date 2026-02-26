;;; +org-minimal.el -*- lexical-binding: t; -*-

(after! org
  (setq org-startup-folded t)
  (setq scratch-file (concat org-directory "scratch.org"))
  (setq archive-file (concat org-directory "archive.org"))

  ;; Archive
  (setq org-archive-location (concat archive-file "::* From %s"))
  (setq org-archive-reversed-order t)

  ;; Capture: dump everything into scratch.org
  (setq org-capture-templates
        '(("s" "Scratch" entry (file scratch-file) "* %?\n%i\n" :jump-to-captured t)
          ("S" "Scratch (link)" entry (file scratch-file) "* %?\n%a\n%i\n" :jump-to-captured t)
          ("l" "Link" item (file+headline scratch-file "Links") "- %?\n" :jump-to-captured t)))

  ;; Refile: any top-level heading in org/ (excluding roam)
  (setq org-refile-targets
        '((nil :maxlevel . 2)  ; current buffer up to level 2
          (org-refile-files :maxlevel . 1)))  ; other files top-level only

  (defun org-refile-files ()
    "Return list of org files for refile targets (excluding roam/)."
    (seq-remove
     (lambda (f) (string-match-p "/roam/" f))
     (directory-files-recursively org-directory "\\.org$")))

  (setq org-refile-use-outline-path 'file)  ; show file path in refile menu
  (setq org-outline-path-complete-in-steps nil)  ; fuzzy match full path

  ;; TODO keywords
  (setq org-todo-keywords (quote((sequence "TODO⚑" "SOMEDAY⚐" "IN-PROGRESS/WAITING⚐" "|" "DONE✔" "CANCELED✘"))))
  (setq org-todo-keyword-faces
        '(("TODO⚑" . "deep sky blue")
          ("SOMEDAY⚐" . "cornflower blue")
          ("IN-PROGRESS/WAITING⚐" . "orange")
          ("DONE✔" . "medium spring green")
          ("CANCELED✘" . (:foreground "red"))))
  )
