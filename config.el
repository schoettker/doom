;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Lennart Schoettker"
      user-mail-address "lennartschoettker@hotmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;; (setq doom-font (font-spec :family "DejaVu Sans Mono" :size 36))
(setq doom-font (font-spec :family "MesloLGS NF" :size 20)) ;; taken from Monaco Linux https://github.com/hbin/top-programming-fonts/blob/master/Monaco-Linux.ttf
;; (setq doom-font (font-spec :family "JetBrains Mono" :size 16)) ;; taken from Monaco Linux https://github.com/hbin/top-programming-fonts/blob/master/Monaco-Linux.ttf
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(if (not (display-graphic-p))
    (setq doom-theme 'doom-monokai-octagon)
  (setq doom-theme 'doom-palenight))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(setq display-line-numbers-type 'relative)
;; (setq display-line-numbers-current-absolute nil)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;
;;
;;
;;
(use-package! winum :config (winum-mode))
;; Load functions
(load! "+functions")
;; Load keybindings
(load! "+keybindings")
;;
;; Scroll behavior
(setq scroll-margin 5)
(setq scroll-conservatively most-positive-fixnum)

;; Default Tab with
(setq-default tab-width 2)
(setq tab-width 2)

;; Dont automatically continue comments after RET/o/O
(setq +default-want-RET-continue-comments nil)
(setq +evil-want-o/O-to-continue-comments nil)

(after! lsp-mode
  ;; https://github.com/emacs-lsp/lsp-mode/issues/3577#issuecomment-1709232622
  (delete 'lsp-terraform lsp-client-packages))
(setenv "PATH" (concat (getenv "PATH") ":/Users/lschoettker/dev/go/bin"))



;; (use-package obsidian
;;   :demand t
;;   :config
;;   (obsidian-specify-path "~/dizzy")
;;   (global-obsidian-mode t)
;;   :custom
;;   ;; This directory will be used for `obsidian-capture' if set.
;;   (obsidian-inbox-directory "Inbox")
;;   ;; Create missing files in inbox? - when clicking on a wiki link
;;   ;; t: in inbox, nil: next to the file with the link
;;   ;; default: t
;;                                         ;(obsidian-wiki-link-create-file-in-inbox nil)
;;   ;; The directory for daily notes (file name is YYYY-MM-DD.md)
;;   (obsidian-daily-notes-directory "Daily Notes")
;;   ;; Directory of note templates, unset (nil) by default
;;                                         ;(obsidian-templates-directory "Templates")
;;   ;; Daily Note template name - requires a template directory. Default: Daily Note Template.md
;;                                         ;(setq obsidian-daily-note-template "Daily Note Template.md")
;;   :bind (:map obsidian-mode-map
;;               ;; Replace C-c C-o with Obsidian.el's implementation. It's ok to use another key binding.
;;               ("C-c C-o" . obsidian-follow-link-at-point)
;;               ;; Jump to backlinks
;;               ("C-c C-b" . obsidian-backlink-jump)
;;               ;; If you prefer you can use `obsidian-insert-link'
;;               ("C-c C-l" . obsidian-insert-wikilink)))
