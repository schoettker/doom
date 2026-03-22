;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! ag)
(package! winum)

(package! exec-path-from-shell)
(package! magit-delta :recipe (:host github :repo "dandavison/magit-delta"))

(package! shell-maker)
(package! acp)
(package! agent-shell)

;; Structural diffs
(package! difftastic :recipe (:host github :repo "pkryger/difftastic.el"))

;; Database
(package! pgmacs :recipe (:host github :repo "emarsden/pgmacs"))

;; Auto-commit org files on save
(package! git-auto-commit-mode)

;; Org modernization
(package! org-modern)
(package! visual-fill-column)
(package! mixed-pitch)
(package! org-appear)
