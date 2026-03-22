;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! ag)
(package! winum)

(package! exec-path-from-shell)
(package! magit-delta :recipe (:host github :repo "dandavison/magit-delta"))

(package! shell-maker)
(package! acp)
(package! agent-shell)

;; Database
(package! pgmacs :recipe (:host github :repo "emarsden/pgmacs"))
;; Org modernization
(package! org-modern)
(package! visual-fill-column)
(package! mixed-pitch)
(package! org-appear)
