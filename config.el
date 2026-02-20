;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Lennart Schoettker"
      user-mail-address "lennartschoettker@hotmail.com")

;; (setq doom-font (font-spec :family "DejaVu Sans Mono" :size 36))
;; (setq doom-font (font-spec :family "MesloLGS NF" :size 20))
;; (setq doom-font (font-spec :family "Excalifont" :size 20))
;; (setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
(setq doom-font (font-spec :family "JetBrains Mono" :size 20)
      doom-big-font (font-spec :family "JetBrains Mono" :size 36)
      doom-variable-pitch-font (font-spec :family "Iowan Old Style" :size 26))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (if (not (display-graphic-p))
;;     (setq doom-theme 'doom-monokai-octagon))
;;   (setq doom-theme 'doom-snazzy)
;; (setq doom-theme 'doom-vibrant)
;; (setq doom-theme 'doom-monokai-octagon)
;; (load-theme 'doom-monokai-octagon)
;; (setq doom-theme 'doom-tomorrow-day) ;; light theme
(setq doom-theme 'doom-xcode)

(setq display-line-numbers-type 'relative)

(setq org-directory "~/org/")
(setq org-roam-directory "~/org/roam/")

(setq-default org-download-image-dir "~/org/assets")
(setq org-hide-emphasis-markers t)

(use-package! winum :config (winum-mode))

;; Load configuration modules
(load! "+functions")
(load! "+keybindings")
(load! "+org-minimal")
(load! "+theme")

;; Scroll behavior
(setq scroll-margin 5)
(setq scroll-conservatively most-positive-fixnum)

;; Tab width
(setq-default tab-width 2)
(setq tab-width 2)

;; Don't automatically continue comments after RET/o/O
(setq +default-want-RET-continue-comments nil)
(setq +evil-want-o/O-to-continue-comments nil)

(setenv "PATH" (concat (getenv "PATH") ":/Users/lschoettker/dev/go/bin"))

;; Tree-sitter grammar sources
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
        (bash-mode . bash-ts-mode)))

(setq tool-bar-mode nil)

(defvar org-babel-default-header-args:cpp '((:flags . "-std=c++20")))

(add-to-list 'default-frame-alist '(fullscreen . maximized))


(use-package! exec-path-from-shell :config (exec-path-from-shell-initialize))


;; Needs brew install git-delta
;; (setq initial-buffer-choice "~/org/world.org")
(use-package! magit-delta
  :hook (magit-mode . magit-delta-mode))

(setq +doom-dashboard-pwd-policy "~")

(require 'acp)
(require 'agent-shell)

(setq agent-shell-anthropic-default-model-id "claude-opus-4-6")
(setq agent-shell-session-strategy 'prompt)
