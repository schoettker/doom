;;; +org-modern.el --- Paper-like org-mode appearance -*- lexical-binding: t; -*-

;; visual-fill-column disabled — was constraining table width
;; (use-package! visual-fill-column
;;   :hook (org-mode . visual-fill-column-mode)
;;   :init
;;   (setq-default visual-fill-column-width 100
;;                 visual-fill-column-center-text t))

;; Soft-wrap lines within the fill column
(add-hook 'org-mode-hook #'visual-line-mode)

;; Use variable-pitch (Iowan Old Style) for prose, keep monospace for code
(use-package! mixed-pitch
  :hook (org-mode . mixed-pitch-mode))

;; Generous line spacing — airy, magazine feel
(add-hook 'org-mode-hook (lambda () (setq-local line-spacing 0.25)))

;; Hide line numbers and fringes in org — clean paper look
(add-hook 'org-mode-hook (lambda ()
                           (display-line-numbers-mode -1)
                           (setq-local left-fringe-width 0
                                       right-fringe-width 0)
                           (set-window-fringes nil 0 0)))

;; Reveal emphasis markers on cursor hover so you can still edit them
(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autolinks t
        org-appear-autosubmarkers t
        org-appear-autoentities t
        org-appear-inside-latex t
        org-appear-delay 0.2))

;; Left margin padding for a bit of breathing room from the window edge
;; (add-hook 'org-mode-hook (lambda ()
;;                            (setq-local left-margin-width 1
;;                                        right-margin-width 1)
;;                            (set-window-buffer nil (current-buffer))))

;; org-modern: sleek bullets, tables, and block styling
(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star '("◉" "○" "◈" "◇" "▸")
        org-modern-list '((?- . "•") (?+ . "➤") (?* . "◦"))
        org-modern-checkbox '((?X . "☑") (?- . "◧") (?\s . "☐"))
        org-modern-block-fringe nil
        org-modern-block-name '("" . "")
        org-modern-table nil
        org-modern-horizontal-rule "──────────────────────────────────────────"
        org-modern-todo t
        org-modern-tag t
        org-modern-priority t
        org-modern-timestamp t
        org-modern-keyword nil))

;; ── Faces ──────────────────────────────────────────────────────────

;; Headings: warm, editorial palette on gruvbox
(custom-set-faces!
  '(org-level-1 :height 1.5  :weight bold      :inherit variable-pitch :foreground "#d65d0e")
  '(org-level-2 :height 1.3  :weight bold      :inherit variable-pitch :foreground "#b8bb26")
  '(org-level-3 :height 1.18 :weight semi-bold :inherit variable-pitch :foreground "#83a598")
  '(org-level-4 :height 1.08 :weight semi-bold :inherit variable-pitch :foreground "#d3869b")
  '(org-level-5 :height 1.04 :weight normal    :inherit variable-pitch :foreground "#fabd2f")
  '(org-document-title :height 1.8 :weight bold :inherit variable-pitch :foreground "#fbf1c7")
  '(org-document-info  :height 1.1 :inherit variable-pitch :foreground "#a89984"))

;; Src blocks and code: monospace with subtle tinted background
(custom-set-faces!
  '(org-block            :inherit fixed-pitch :background "#1d2021" :extend t)
  '(org-block-begin-line :inherit fixed-pitch :foreground "#7c6f64" :background "#282828" :extend t :height 0.85)
  '(org-block-end-line   :inherit fixed-pitch :foreground "#7c6f64" :background "#282828" :extend t :height 0.85)
  '(org-code             :inherit fixed-pitch :foreground "#fe8019" :background "#1d2021")
  '(org-verbatim         :inherit fixed-pitch :foreground "#8ec07c" :background "#1d2021")
  '(org-table            :inherit fixed-pitch :foreground "#d5c4a1" :height 0.8))

;; Dim metadata so content stands out
(custom-set-faces!
  '(org-drawer         :foreground "#504945" :height 0.85)
  '(org-property-value :foreground "#665c54" :height 0.85 :inherit fixed-pitch)
  '(org-special-keyword :foreground "#504945" :height 0.85)
  '(org-meta-line      :foreground "#504945" :height 0.85)
  '(org-tag            :foreground "#7c6f64" :weight normal :height 0.85))

;; Links: clean underline, no box
(custom-set-faces!
  '(org-link :foreground "#83a598" :underline t :weight normal))

;; Quote blocks: italicized with a left-border feel
(custom-set-faces!
  '(org-quote :inherit variable-pitch :slant italic :foreground "#bdae93" :background "#282828" :extend t))

;; Org settings for visual polish
(after! org
  (setq org-fontify-whole-heading-line t
        org-fontify-quote-and-verse-blocks t
        org-ellipsis " ▾"
        org-pretty-entities t
        org-auto-align-tags nil
        org-tags-column 0
        org-agenda-tags-column 0
        org-catch-invisible-edits 'show-and-error
        org-insert-heading-respect-content t))
