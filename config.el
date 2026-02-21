;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Raise GC threshold to reduce pauses during editing (256MB)
(setq gc-cons-threshold (* 256 1024 1024))

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
(setq doom-theme 'doom-gruvbox)

(setq display-line-numbers-type 'relative)

(setq org-directory "~/org/")
(setq org-roam-directory "~/org/roam/")

(setq-default org-download-image-dir "~/org/assets")
(setq org-hide-emphasis-markers t)

(use-package! winum
  :defer t
  :commands (winum-select-window-1 winum-select-window-2
             winum-select-window-3 winum-select-window-4
             winum-select-window-5)
  :init (add-hook 'doom-first-buffer-hook #'winum-mode))

;; Load configuration modules
(load! "+functions")
(load! "+keybindings")
(load! "+org-minimal")
(load! "+theme")

;; Disable workspace session persistence (no auto-save/restore).
(after! persp-mode
  (setq persp-auto-save-opt 0)
  (setq persp-auto-resume-time -1))

;; Exclude workspace files from recentf (SPC f r)
(after! recentf
  (add-to-list 'recentf-exclude "/\\.config/emacs/\\.local/"))

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

;; (setq +doom-dashboard-pwd-policy "~")

;; Daemon mode optimizations for instant first emacsclient frame.
(when (daemonp)
  (add-hook 'emacs-startup-hook
            (lambda ()
              ;; Clear server-after-make-frame-hook — it contains blocking
              ;; chainers that cause a blank screen + keypress wait.
              (setq server-after-make-frame-hook nil)
              ;; Re-apply theme and fonts on each new client frame.
              (add-hook 'after-make-frame-functions
                        (lambda (frame)
                          (with-selected-frame frame
                            (doom-init-fonts-h)
                            (doom-init-theme-h))))
              ;; Fix: first file opened in a new session lands in
              ;; fundamental-mode. Detect and re-apply correct mode.
              (defvar doom-daemon--fix-mode-guard nil)
              (defun doom-daemon--fix-fundamental-mode-h (_)
                (when (and buffer-file-name
                          (eq major-mode 'fundamental-mode)
                          (not doom-daemon--fix-mode-guard))
                  (let ((doom-daemon--fix-mode-guard t))
                    (normal-mode))))
              (add-hook 'window-buffer-change-functions
                        #'doom-daemon--fix-fundamental-mode-h)
              ;; Warm up: create an invisible frame, open a file, switch
              ;; buffers — exercises all deferred hooks naturally.
              (let ((warmup-frame (make-frame '((visibility . nil)
                                                (width . 80)
                                                (height . 24)))))
                (with-selected-frame warmup-frame
                  (find-file (expand-file-name "config.el" doom-user-dir))
                  (switch-to-buffer "*scratch*")
                  (kill-buffer (get-file-buffer
                                (expand-file-name "config.el" doom-user-dir))))
                (delete-frame warmup-frame)))
            100))

(use-package! acp :defer t)
(use-package! agent-shell
  :defer t
  :commands agent-shell
  :config
  (setq agent-shell-anthropic-default-model-id "claude-opus-4-6")
  (setq agent-shell-session-strategy 'prompt))
