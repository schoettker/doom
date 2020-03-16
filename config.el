;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Bront" :size 18))
(setq doom-themes-enable-bold nil)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
;; (setq doom-theme 'doom-solarized-light)
(setq doom-theme 'doom-one)

;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/library/Dropbox/org/")

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(setq display-line-numbers-type 'relative)
(setq display-line-numbers-current-absolute nil)

(defun add-semicolon ()
  (interactive)
  (end-of-line)
  (when (not (looking-back ";"))
    (insert ";"))
  (evil-first-non-blank))

(def-package! winum :config (winum-mode))
(after! winum
  (map! :leader
        (:desc "win 1" :n "1" #'winum-select-window-1)
        (:desc "win 2" :n "2" #'winum-select-window-2)
        (:desc "win 3" :n "3" #'winum-select-window-3)
        (:desc "win 4" :n "4" #'winum-select-window-4)
        (:desc "win 5" :n "5" #'winum-select-window-5)
        ))

(defun open-termite ()
  (interactive "@")
  (shell-command (concat "termite"
                         " > /dev/null 2>&1 & disown") nil nil))
(map! :leader
      (:n ":" #'doom/open-scratch-buffer)
      (:n "x" #'counsel-M-x)
      (:desc "prev buffer" :n "TAB" #'mode-line-other-buffer)
      (:desc "go to char" :n "SPC" #'avy-goto-word-or-subword-1)
      (:desc "add ;" :n "RET" #'add-semicolon-at-eol)
      (:desc "swiper" :n "/" #'counsel-grep-or-swiper)
      (:desc "kill buffer" :n "d" #'kill-this-buffer)
      (:desc "apps" :prefix "a"
        :desc "ag" :n "a" #'ag
        :desc "dired" :n "d" #'dired
        :desc "termite" :n "t" #'open-termite)
      (:desc "e" :prefix "e"
        :desc "next error" :n "n" #'flycheck-next-error
        :desc "prev error" :n "p" #'flycheck-previous-error
        :desc "termite" :n "t" #'open-termite)
      (:desc "git" :prefix "g"
        :desc "status" :n "s" #'magit-status)
      (:desc "insert" :prefix "i"
        :desc "org table" :n "t" #'org-table-create)
      (:desc "Project" :prefix "p"
        :desc "search in project" :n "/" #'+ivy/project-search
        :desc "find file" :n "p" #'+ivy/projectile-find-file
        :desc "find file" :n "f" #'+ivy/projectile-find-file
        :desc "switch project" :n "s" #'counsel-projectile-switch-project)
      (:desc "open" :prefix "o"
        :desc "open config" :n "c" #'(lambda () (interactive) (find-file "~/.doom.d/config.el")))
      (:desc "window" :prefix "w"
        :desc "close workspace" :n "D" #'eyebrowse-close-window-config
        :desc "maximize buffer" :n "m" #'doom/window-maximize-buffer))

(map! :nvime "C-`" #'+popup/raise)
(map! :nvime "C-y" #'yank)
(map! :nvme "\\" #'evil-ex-nohighlight)
(def-package! eyebrowse
  :defer 2
  :config (eyebrowse-mode t)
                                        ;(set-face-foreground 'eyebrowse-mode-line-active "medium turquoise")
  (set-face-foreground 'eyebrowse-mode-line-active "purple")
  (setq eyebrowse-mode-line-separator "|")
  (setq eyebrowse-new-workspace "*doom*")
  (map!
   :nvie "M-1" #'eyebrowse-switch-to-window-config-1
   :nvie "M-2" #'eyebrowse-switch-to-window-config-2
   :nvie "M-3" #'eyebrowse-switch-to-window-config-3
   :nvie "M-4" #'eyebrowse-switch-to-window-config-4
   :nvie "M-5" #'eyebrowse-switch-to-window-config-5
   )
  )

;; (def-package! smooth-scroll
;;   :config (smooth-scroll-mode 1))
(setq scroll-margin 5)
(setq scroll-conservatively most-positive-fixnum)

(defun my-change-number-at-point (change)
  (let ((number (number-at-point))
        (point (point)))
    (when number
      (progn
        (forward-word)
        (search-backward (number-to-string number))
        (replace-match (number-to-string (funcall change number)))
        (goto-char point)))))
(defun my-increment-number-at-point ()
                                        ;"Increment number at point like vim's C-a"
  (interactive)
  (my-change-number-at-point '1+))
(defun my-decrement-number-at-point ()
  "Decrement number at point like vim's C-x"
  (interactive)
  (my-change-number-at-point '1-))

(map! :nvime "C-c a" #'my-increment-number-at-point)
(map! :nvime "C-c x" #'my-decrement-number-at-point)

(map! :nv "j" #'evil-next-visual-line)
(map! :nv "k" #'evil-previous-visual-line)

(setq undo-tree-auto-save-history nil)
(setq +doom-dashboard-banner-file (expand-file-name "logo.png" doom-private-dir))



;; Wrap experiment
;; (setq default-fill-column 80)
;; (visual-line-mode 1)
;; (+word-wrap-mode)
;; (setq +word-wrap--enable-visual-fill-mode t)
(global-visual-line-mode 1)

(after! magit
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-save-repository-buffers t)
  )

(defun create-buffer(name)
  (interactive "sEnter name for new buffer: ")
  (switch-to-buffer
   (find-file (concat "/tmp/" name))))

;; (remove-hook 'org-mode-hook #'auto-fill-mode)
;; (add-hook 'org-mode-hook (lambda () (org-indent-mode 0)))
;; (setq org-hide-leading-stars-before-indent-mode nil)
(add-hook 'org-mode-hook (lambda () (auto-fill-mode 0)))
(after! org
  (setq org-adapt-indentation nil)
  (setq org-hide-leading-stars nil)
  (setq org-hide-leading-stars-before-indent-mode nil)
  (setq org-startup-indented nil)
  (defun export-to-html-light ()
    (interactive)
    (if (not (boundp 'sakura-light))
      (load-file "~/.doom.d/+misc/sakura-light.el")
      (setq org-html-head sakura-light)
      )
    (org-html-export-to-html))

  (defun export-to-html-dark ()
    (interactive)
    (if (not (boundp 'sakura-dark))
        (load-file "~/.doom.d/+misc/sakura-dark.el")
      (setq org-html-head sakura-dark)
      )
    (org-html-export-to-html))

  )
