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
;; (setq doom-font (font-spec :family "MesloLGS NF" :size 20)) ;; taken from Monaco Linux https://github.com/hbin/top-programming-fonts/blob/master/Monaco-Linux.ttf


(setq doom-font (font-spec :family "JetBrains Mono" :size 22)
      doom-big-font (font-spec :family "JetBrains Mono" :size 36)
      doom-variable-pitch-font (font-spec :family "Iowan Old Style" :size 26)
      ;;doom-symbol-font (font-spec :family "JuliaMono")
      ;;doom-emoji-font (font-spec :family "Twitter Color Emoji") ; Just used by me
      ;;doom-serif-font (font-spec :family "IBM Plex Mono" :size 22 :weight 'light)
      )


;; (setq doom-font (font-spec :family "Excalifont" :size 20)) ;; taken from Monaco Linux https://github.com/hbin/top-programming-fonts/blob/master/Monaco-Linux.ttf
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
;; (if (not (display-graphic-p))
;;     (setq doom-theme 'doom-monokai-octagon))
;;   (setq doom-theme 'doom-snazzy)
;; (setq doom-theme 'doom-vibrant)
;; (setq doom-theme 'doom-monokai-octagon)
;; (load-theme 'doom-monokai-octagon)
;; (setq doom-theme 'doom-tomorrow-day) ;; light theme
(setq doom-theme 'doom-xcode)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(setq display-line-numbers-type 'relative)
;; (setq display-line-numbers-current-absolute nil)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq org-roam-directory "~/org/roam/")

(setq-default org-download-image-dir "~/org/assets")
(setq org-hide-emphasis-markers t)

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
;; Load org mode settings
;; (load! "+org")
(load! "+org-minimal")
(load! "+theme")
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

;; https://emacs.stackexchange.com/questions/62376/slow-markdown-mode-as-emacs-spends-lots-of-time-fontifying
;; (defconst markdown-regex-italic
;;   "\\(?:^\\|[^\\]\\)\\(?1:\\(?2:[_]\\)\\(?3:[^ \n\t\\]\\|[^ \n\t]\\(?:.\\|\n[^\n]\\)[^\\ ]\\)\\(?4:\\2\\)\\)")
;; and/or
;; (defconst markdown-regex-gfm-italic
;;   "\\(?:^\\|[^\\]\\)\\(?1:\\(?2:[_]\\)\\(?3:[^ \\]\\2\\|[^ ]\\(?:.\\|\n[^\n]\\)\\)\\(?4:\\2\\)\\)")

;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
;; On new systems run M-x treesit-install-language-grammar to get markdown grammar installed
;; and check that everything works with (treesit-language-available-p 'markdown)
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
        (bash-mode . bash-ts-mode)
        ;; check here if its exists https://github.com/emacs-mirror/emacs/tree/master/lisp/textmodes
        ;; (markdown-mode . tree-sitter-mode) ;; doesnt seem to exist yet
        ))

(setq tool-bar-mode nil)

;; Set initial frame size and position
;; There are some approaches here https://www.reddit.com/r/emacs/comments/9c0a4d/tip_setting_initial_frame_size_and_position/
;; But manually finding some sizes seems to work best for me
;; For horizontal, 27":
;; (setq default-frame-alist '((top . 70) (left . 70) (width . 200) (height . 50)))

(add-hook 'c++-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (concat "g++ -std=c++17 " buffer-file-name))
            (flycheck-mode -1)
            ))


(defun eshell-buffer-p (buffer)
  (string-match-p "^\\*eshell*" (buffer-name buffer)))
(push #'eshell-buffer-p doom-real-buffer-functions)

(push '("\\*compilation\\*" . (nil (reusable-frames . t))) display-buffer-alist)


(set-file-template!
  "/codeforces/.+\\.cpp$"
  :trigger
  "sol")


;; (defun compileandrun()
;;   (interactive)
;;   (let* ((src (file-name-nondirectory (buffer-file-name)))
;;          (exe (file-name-sans-extension src)))
;;     (compile (concat "g++ -std=c++17 " src " -o " exe " && timeout 1s ./" exe ))))

;; (defun execute-c-program ()
;;   (interactive)
;;   (defvar foo)
;;   (setq foo (concat "g++ " (buffer-name) " && ./a.out" ))
;;   (shell-command foo))

(defvar org-babel-default-header-args:cpp '((:flags . "-std=c++20")))

(add-to-list 'default-frame-alist '(fullscreen . maximized))


(use-package! exec-path-from-shell :config (exec-path-from-shell-initialize))


;; (setq line-spacing 0.4)
;; (face-remap-add-relative 'default :family "Iowan Old Style" :height 240)  ;; or some other font
;; (visual-line-mode +1)
;; (olivetti-mode +1)


;; ressoures
;; https://tecosaur.github.io/emacs-config/config.html#theme
;; https://www.reddit.com/r/emacs/comments/hnf3cw/my_orgmode_agenda_much_better_now_with_category/
;;         https://github.com/psamim/dotfiles/blob/master/doom/config.el#L73
;; https://github.com/jacmoe/.doom.d/blob/master/config.el

;; Needs brew install git-delta
(use-package! magit-delta
  :hook (magit-mode . magit-delta-mode))


;; Beautiful Org + Writeroom

;; (defvar mixed-pitch-modes '(org-mode LaTeX-mode markdown-mode gfm-mode Info-mode)
(defvar mixed-pitch-modes '(LaTeX-mode markdown-mode gfm-mode Info-mode)
  "Modes that `mixed-pitch-mode' should be enabled in, but only after UI initialisation.")
(defun init-mixed-pitch-h ()
  "Hook `mixed-pitch-mode' into each mode in `mixed-pitch-modes'.
Also immediately enables `mixed-pitch-modes' if currently in one of the modes."
  (when (memq major-mode mixed-pitch-modes)
    (mixed-pitch-mode 1))
  (dolist (hook mixed-pitch-modes)
    (add-hook (intern (concat (symbol-name hook) "-hook")) #'mixed-pitch-mode)))
(add-hook 'doom-init-ui-hook #'init-mixed-pitch-h)
;; (setq! variable-pitch-serif-font (font-spec :family "Alegreya" :size 27))
(setq! variable-pitch-serif-font (font-spec :family "Iowan Old Style" :size 27))

(after! mixed-pitch
  (setq mixed-pitch-set-height t)
  ;; (set-face-attribute 'variable-pitch-serif nil :font variable-pitch-serif-font)
  (defun mixed-pitch-serif-mode (&optional arg)
    "Change the default face of the current buffer to a serifed variable pitch, while keeping some faces fixed pitch."
    (interactive)
    (let ((mixed-pitch-face 'variable-pitch-serif))
      (mixed-pitch-mode (or arg 'toggle)))))


(setq +zen-text-scale 0.8)



(defvar +zen-serif-p t
  "Whether to use a serifed font with `mixed-pitch-mode'.")
(defvar +zen-org-starhide nil
  "The value `org-modern-hide-stars' is set to.")

(after! writeroom-mode
  (defvar-local +zen--original-org-indent-mode-p nil)
  (defvar-local +zen--original-mixed-pitch-mode-p nil)
  (defun +zen-enable-mixed-pitch-mode-h ()
    "Enable `mixed-pitch-mode' when in `+zen-mixed-pitch-modes'."
    (when (apply #'derived-mode-p +zen-mixed-pitch-modes)
      (if writeroom-mode
          (progn
            (setq +zen--original-mixed-pitch-mode-p mixed-pitch-mode)
            (funcall (if +zen-serif-p #'mixed-pitch-serif-mode #'mixed-pitch-mode) 1))
        (funcall #'mixed-pitch-mode (if +zen--original-mixed-pitch-mode-p 1 -1)))))
  (defun +zen-prose-org-h ()
    "Reformat the current Org buffer appearance for prose."
    (when (eq major-mode 'org-mode)
      (setq
       display-line-numbers nil
       visual-fill-column-width 60
       line-spacing 0.4
       org-adapt-indentation nil)
      (when (featurep 'org-modern)
        (setq-local org-modern-star '("🙘" "🙙" "🙚" "🙛")
                    ;; org-modern-star '("🙐" "🙑" "🙒" "🙓" "🙔" "🙕" "🙖" "🙗")
                    org-modern-hide-stars +zen-org-starhide)
        (org-modern-mode -1)
        (org-modern-mode 1))
      (setq
       +zen--original-org-indent-mode-p org-indent-mode)
      (org-indent-mode -1)))
  (defun +zen-nonprose-org-h ()
    "Reverse the effect of `+zen-prose-org'."
    (when (eq major-mode 'org-mode)
      (when (bound-and-true-p org-modern-mode)
        (org-modern-mode -1)
        (org-modern-mode 1))
      (when +zen--original-org-indent-mode-p (org-indent-mode 1))))
  (pushnew! writeroom--local-variables
            'display-line-numbers
            'visual-fill-column-width
            'org-adapt-indentation
            'org-modern-mode
            'org-modern-star
            'org-modern-hide-stars)
  (add-hook 'writeroom-mode-enable-hook #'+zen-prose-org-h)
  (add-hook 'writeroom-mode-disable-hook #'+zen-nonprose-org-h))

;; (setq initial-buffer-choice "~/org/world.org")

(use-package! claude-code-ide
  :config
  (claude-code-ide-emacs-tools-setup)  ; Optionally enable Emacs MCP tools
  (setq claude-code-ide-terminal-backend 'vterm) ;; Use vterm (or eat)
  )


