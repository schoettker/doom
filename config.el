;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Let Doom's gcmh manage GC — just tune the high-water mark (64MB)
(setq gcmh-high-cons-threshold (* 64 1024 1024))

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
  :hook (after-init . winum-mode))


;; Load configuration modules
(load! "+functions")
(load! "+keybindings")
(load! "+org-minimal")
(load! "+org-modern")
(load! "+theme")

;; Disable workspace session persistence (no auto-save/restore).
(after! persp-mode
  (setq persp-auto-save-opt 0)
  (setq persp-auto-resume-time -1))

;; Exclude workspace files from recentf (SPC f r)
(after! recentf
  (add-to-list 'recentf-exclude "/\\.config/emacs/\\.local/"))

;; Force terminal cursor shape changes (block/bar/underline per evil state).
;; The tty module's evil-terminal-cursor-changer doesn't detect Ghostty/Kitty
;; when connecting via emacsclient to a daemon (env vars aren't inherited).
;; Both terminals support xterm-style DECSCUSR sequences.
(setq etcc-term-type-override 'xterm)

;; Faster which-key popup (default 1.0s feels sluggish)
(setq which-key-idle-delay 0.3)

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

(tool-bar-mode -1)

(defvar org-babel-default-header-args:cpp '((:flags . "-std=c++20")))

(add-to-list 'default-frame-alist '(fullscreen . maximized))


(use-package! exec-path-from-shell :config (exec-path-from-shell-initialize))


;; Needs brew install git-delta
;; (setq initial-buffer-choice "~/org/world.org")
(use-package! magit-delta
  :hook (magit-mode . magit-delta-mode))

;; Magit performance tuning (helps on large monorepos)
(after! magit
  ;; Don't auto-refresh status buffer on every file save — use `gr` to refresh manually.
  ;; Big perf win in large repos where refresh triggers expensive git operations.
  (setq magit-refresh-status-buffer nil)

  ;; Skip computing related refs in commit views (expensive in repos with many refs)
  (setq magit-revision-insert-related-refs nil)

  ;; Trim status buffer headers: remove tags (slow) and recent commits (expensive)
  (setq magit-status-headers-hook
        '(magit-insert-error-header
          magit-insert-diff-filter-header
          magit-insert-head-branch-header
          magit-insert-upstream-branch-header
          magit-insert-push-branch-header))

  ;; Trim status sections: remove recent-commits and unpushed/unpulled sections
  ;; that scan the full DAG. Keep untracked/unstaged/staged which are always fast.
  (setq magit-status-sections-hook
        '(magit-insert-status-headers
          magit-insert-merge-log
          magit-insert-rebase-sequence
          magit-insert-am-sequence
          magit-insert-sequencer-sequence
          magit-insert-bisect-output
          magit-insert-bisect-rest
          magit-insert-bisect-log
          magit-insert-untracked-files
          magit-insert-unstaged-changes
          magit-insert-staged-changes
          magit-insert-stashes
          magit-insert-unpushed-to-pushremote
          magit-insert-unpushed-to-upstream)))

;; (setq +doom-dashboard-pwd-policy "~")

(use-package! acp :defer t)
(use-package! agent-shell
  :defer t
  :commands agent-shell
  :config
  (setq agent-shell-anthropic-default-model-id "claude-opus-4-6")
  (setq agent-shell-session-strategy 'prompt))


;; difftastic — structural diffs in magit
(use-package! difftastic
  :defer t
  :config
  ;; Strip background colors — use only foreground for added/removed
  (setq difftastic-normal-colors-vector
        (vector
         (aref ansi-color-normal-colors-vector 0) ; black
         'diff-indicator-removed                   ; red (no bg)
         'diff-indicator-added                     ; green (no bg)
         (aref ansi-color-normal-colors-vector 3)  ; yellow
         (aref ansi-color-normal-colors-vector 4)  ; blue
         (aref ansi-color-normal-colors-vector 5)  ; magenta
         (aref ansi-color-normal-colors-vector 6)  ; cyan
         (aref ansi-color-normal-colors-vector 7))) ; white
  (setq difftastic-bright-colors-vector
        (vector
         (aref ansi-color-bright-colors-vector 0)
         'diff-indicator-removed
         'diff-indicator-added
         (aref ansi-color-bright-colors-vector 3)
         (aref ansi-color-bright-colors-vector 4)
         (aref ansi-color-bright-colors-vector 5)
         (aref ansi-color-bright-colors-vector 6)
         (aref ansi-color-bright-colors-vector 7)))
  (setq difftastic-highlight-alist nil)
  ;; Force side-by-side and use full frame width
  ;; (setq difftastic-requested-window-width-function
  ;;       (lambda () (frame-width)))
  :init
  (after! magit-diff
    (transient-append-suffix 'magit-diff '(-1 -1)
      [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
       ("S" "Difftastic show" difftastic-magit-show)])))

;; pgmacs — interactive Postgres browser
(use-package! pgmacs
  :defer t
  :commands pgmacs)

(setq initial-buffer-choice "~/org/dashboard.org")
