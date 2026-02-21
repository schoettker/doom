;;; +theme.el -*- lexical-binding: t; -*-

;; Customizations applied on top of doom-gruvbox.

(add-hook 'doom-load-theme-hook
          (defun customize-doom-gruvbox ()
            (when (eq doom-theme 'doom-gruvbox)

              ;; Cursor
              (set-face-attribute 'cursor nil :foreground "#fa7d00" :background "#fa0032")

              ;; Evil state indicators in modeline
              (set-face-attribute 'doom-modeline-evil-insert-state nil :foreground "#b8bb26" :weight 'bold)
              (set-face-attribute 'doom-modeline-evil-emacs-state nil :foreground "#b16286" :weight 'bold)
              (set-face-attribute 'doom-modeline-evil-normal-state nil :foreground "#83a598" :weight 'bold)
              (set-face-attribute 'doom-modeline-evil-visual-state nil :foreground "#fbf1c7" :weight 'bold)
              (set-face-attribute 'doom-modeline-evil-replace-state nil :foreground "#fb4934" :weight 'bold)
              (set-face-attribute 'doom-modeline-evil-operator-state nil :foreground "#fabd2f" :weight 'bold)

              ;; Modeline — grey instead of green
              (set-face-attribute 'mode-line nil :background "#504945" :foreground "#d5c4a1")
              (set-face-attribute 'mode-line-inactive nil :background "#3c3836" :foreground "#7c6f64")
              (set-face-attribute 'doom-modeline-buffer-path nil :foreground "#a89984" :weight 'bold)
              (set-face-attribute 'doom-modeline-buffer-major-mode nil :foreground "#a89984" :weight 'bold)
              (set-face-attribute 'doom-modeline-bar nil :background "#504945")
              (set-face-attribute 'doom-modeline-bar-inactive nil :background "#3c3836")
              (set-face-attribute 'doom-modeline-panel nil :background "#504945" :foreground "#ebdbb2"))))

;; diff-hl (vc-gutter) — visible git signs in terminal
;; Applied after diff-hl loads so the faces exist.
(after! diff-hl
  (set-face-attribute 'diff-hl-insert nil :foreground "#b8bb26" :background "#282e24")
  (set-face-attribute 'diff-hl-delete nil :foreground "#fb4934" :background "#2e2828")
  (set-face-attribute 'diff-hl-change nil :foreground "#83a598" :background "#282c2e"))

(after! diff-hl-margin
  (set-face-attribute 'diff-hl-margin-insert nil :foreground "#b8bb26" :background "#282e24")
  (set-face-attribute 'diff-hl-margin-delete nil :foreground "#fb4934" :background "#2e2828")
  (set-face-attribute 'diff-hl-margin-change nil :foreground "#83a598" :background "#282c2e"))
