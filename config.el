;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Lennart Schoettker"
      user-mail-address "lennartschoettker@hotmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Bront" :size 18))
(setq doom-themes-enable-bold nil)
;; (setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-gruvbox)
(setq doom-theme 'doom-horizon)
;; (setq doom-theme 'doom-snazzy)
;; (setq doom-theme 'misterioso)

;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/library/Dropbox/org/")

;; If you want to change the style of line numbers, change this to `relative' or `nil' to disable it:
(setq display-line-numbers-type t)
(setq display-line-numbers-type 'relative)
(setq display-line-numbers-current-absolute nil)
;;
;; Here are some additional functions/macros that could help you configure Doom:
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.


;; transparency
  (set-frame-parameter (selected-frame) 'alpha '(85 85))
  (add-to-list 'default-frame-alist '(alpha 85 85))

(setq scroll-margin 5)
(setq scroll-conservatively most-positive-fixnum)
(setq undo-tree-auto-save-history nil)
(setq +doom-dashboard-banner-file (expand-file-name "logo.png" doom-private-dir))
(setq-default tab-width 2)
(global-visual-line-mode 1)

(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

(use-package! winum :config (winum-mode))


;; (def-package! smooth-scroll
;;   :config (smooth-scroll-mode 1))

(after! magit
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-save-repository-buffers t))

(after! swiper
  (setq swiper-use-visual-line nil)
  (setq swiper-use-visual-line-p (lambda (a) nil)))

(load! "+functions")
(load! "+keybindings")
(load! "+org")
