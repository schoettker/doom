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
;; (setq doom-theme 'doom-monokai-pro)
;; (setq doom-theme 'doom-molokai)
(setq doom-theme 'doom-palenight)
;; (setq doom-theme 'doom-horizon) ; 2nd favorite and good for diffing!
;; (setq doom-theme 'doom-one) ; probably the best diffing

;; (setq doom-theme 'doom-gruvbox)
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

(setq bookmark-default-file (expand-file-name "bookmarks" doom-private-dir))
(setq scroll-margin 5)
(setq scroll-conservatively most-positive-fixnum)
(setq +doom-dashboard-banner-file (expand-file-name "logo.png" doom-private-dir))
(setq-default tab-width 2)
(setq tab-width 2)
(global-visual-line-mode 1)

(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

(use-package! winum :config (winum-mode))

(after! emms
  (emms-all)
  (emms-default-players)
  (setq emms-source-file-default-directory "~/library/music"))

(after! magit
  (setq git-commit-style-convention-checks '(non-empty-second-line))
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-save-repository-buffers t))

(after! undo-tree (setq undo-tree-auto-save-history nil))

(after! swiper
  (setq swiper-use-visual-line nil)
  (setq swiper-use-visual-line-p (lambda (a) nil)))

(use-package leetcode
  :defer 4
  :config
  (setq leetcode-prefer-language "python3")
  (setq leetcode-prefer-sql "mysql")
  (setq leetcode-save-solutions t)
  (setq leetcode-directory "~/dev/ds/leetcode")
  )

(after! web-mode
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(after! typescript-mode
  (setq typescript-indent-level 2)
  )

(use-package! beancount
  :load-path "~/.doom.d/+misc"
  :mode ("\\.beancount\\'" . beancount-mode)
  :config
  ;; (setq beancount-electric-currency t)
  (add-hook 'beancount-mode-hook #'(lambda () (outline-minor-mode)
                                     ;; (outline-hide-body)
                                     ))

  (defun beancount-bal ()
    "Run bean-report bal."
    (interactive)
    (let ((compilation-read-command nil))
      (beancount--run "bean-report"
                      (file-relative-name buffer-file-name) "bal")))

  (map! :map beancount-mode-map
        :i "TAB" #'company-ledger
        :i "C-SPC" #'company-ledger
        :i "C-N" #'company-ledger
        ;; :n "TAB" #'beancount-align-to-previous-number
        ;; :i "TAB" #'beancount-tab-dwim
        )
  )

(use-package! company-ledger
  :ensure company
  :config
  (defun company-ledger (command &optional arg &rest ignored)
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'company-ledger))
      (prefix (and (eq major-mode 'beancount-mode)
                   (company-grab-symbol)
                   (company-ledger--next-line-empty-p)
                   (thing-at-point 'line t)))
      (candidates
       (cl-remove-if-not
        (lambda (c) (company-ledger--fuzzy-word-match arg c))
        (company-ledger--get-all-postings)))
      (sorted t)))

  (after! beancount
    (set-company-backend! 'beancount-mode 'company-ledger)))

(load! "+functions")
(load! "+keybindings")
(load! "+org")

;; (set-cursor-color "magenta")
;; (setq +evil--default-cursor-color "magenta")
(setq evil-normal-state-cursor '(box "magenta")
      evil-insert-state-cursor '(bar "magenta")
      evil-visual-state-cursor '(hollow "magenta"))


(custom-set-faces
  ;; '(default ((t (:background "#292d3e"))))
  ;; '(hl-line ((t (:background "#292d3e"))))
  '(ediff-current-diff-A ((t (:background "#87003f"))))
  '(ediff-fine-diff-A ((t (:background "#c9005e"))))
  '(ediff-current-diff-Ancestor ((t (:background "#263854"))))
  '(ediff-current-diff-B ((t (:background "#008758")))) ;; this is good
  '(ediff-fine-diff-B ((t (:background "#00d188"))))
  '(ediff-current-diff-C ((t (:background "#c4eeff"))))
  '(ediff-even-diff-A ((t (:background "#263854"))))
  '(ediff-even-diff-Ancestor ((t (:background "#263854"))))
  '(ediff-even-diff-B ((t (:background "#263854"))))
  '(ediff-even-diff-C ((t (:background "#263854"))))
  '(ediff-odd-diff-A ((t (:background "#263854"))))
  '(ediff-odd-diff-Ancestor ((t (:background "#263854"))))
  '(ediff-odd-diff-B ((t (:background "#263854"))))
  '(ediff-odd-diff-C ((t (:background "#263854"))))
 )


 ;; ("components/.+\\.js$" . rjsx-mode)
(use-package! rjsx-mode
  :commands rjsx-mode
  :mode "\\.js$"
  :mode "components/.+\\.js$"
  )



(setq +format-with-lsp nil)


(setq comint-prompt-read-only t)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)
(setq scroll-down-aggressively 1)

(setq +format-on-save-enabled-modes
  '(not emacs-lisp-mode  ; elisp's mechanisms are good enough
        sql-mode         ; sqlformat is currently broken
        tex-mode         ; latexindent is broken
        c++-mode
        latex-mode))
