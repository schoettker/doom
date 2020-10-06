;;; +misc/beautify-org-exp.el -*- lexical-binding: t; -*-

(defun beautiful-org ()
  (interactive)

  (setq display-line-numbers nil)
  (variable-pitch-mode t)
  (set-face-bold 'bold 1)
  (setq line-spacing 3)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-link nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-date nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-special-keyword nil :inherit 'fixed-pitch)

  (setq left-margin-width 2)
  (setq right-margin-width 2)
  (set-window-buffer nil (current-buffer))

  (setq
      org-bullets-bullet-list '(" ") ;; no bullets, needs org-bullets package
   ;; org-startup-indented t
      org-ellipsis "  " ;; folding symbol
      org-pretty-entities t
      org-hide-emphasis-markers t
      ;; show actually italicized text instead of /italicized text/
      org-agenda-block-separator ""
      org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t)

(add-hook 'org-mode-hook 'set-buffer-variable-pitch)

(defmacro set-pair-faces (themes consts faces-alist)
  "Macro for pair setting of custom faces.
THEMES name the pair (theme-one theme-two). CONSTS sets the variables like
  ((sans-font \"Some Sans Font\") ...). FACES-ALIST has the actual faces
like:
  ((face1 theme-one-attr theme-two-atrr)
   (face2 theme-one-attr nil           )
   (face3 nil            theme-two-attr)
   ...)"
  (defmacro get-proper-faces ()
    `(let* (,@consts)
       (backquote ,faces-alist)))

  `(setq theming-modifications
         ',(mapcar (lambda (theme)
                     `(,theme ,@(cl-remove-if
                                 (lambda (x) (equal x "NA"))
                                 (mapcar (lambda (face)
                                           (let ((face-name (car face))
                                                 (face-attrs (nth (cl-position theme themes) (cdr face))))
                                             (if face-attrs
                                                 `(,face-name ,@face-attrs)
                                               "NA"))) (get-proper-faces)))))
                   themes)))
(set-pair-faces
 ;; Themes to cycle in
 (doom-molokai solarized-light)

 ;; Variables
 ((bg-white           "#fbf8ef")
  (bg-light           "#222425")
  (bg-dark            "#1c1e1f")
  (bg-darker          "#1c1c1c")
  (fg-white           "#ffffff")
  (shade-white        "#efeae9")
  (fg-light           "#655370")
  (dark-cyan          "#008b8b")
  (region-dark        "#2d2e2e")
  (region             "#39393d")
  (slate              "#8FA1B3")
  (keyword            "#f92672")
  (comment            "#525254")
  (builtin            "#fd971f")
  (purple             "#9c91e4")
  (doc                "#727280")
  (type               "#66d9ef")
  (string             "#b6e63e")
  (gray-dark          "#999")
  (gray               "#bbb")
  (sans-font          "Source Sans Pro")
  (serif-font         "Merriweather")
  (et-font            "EtBembo")
  (sans-mono-font     "Souce Code Pro")
  (serif-mono-font    "Verily Serif Mono"))

 ;; Settings
 ((variable-pitch
   (:family ,sans-font)
   (:family ,et-font
            :background nil
            :foreground ,bg-dark
            :height 1.7))
  (header-line
   (:background nil :inherit nil)
   (:background nil :inherit nil))
  (eval-sexp-fu-flash
   (:background ,dark-cyan
                :foreground ,fg-white)
   nil)
  (eval-sexp-fu-flash-error
   (:background ,keyword
                :foreground ,fg-white)
   nil)
  (hackernews-link-face
   (:foreground ,slate
                :inherit variable-pitch
                :height 1.2)
   nil)
  (hackernews-comment-count-face
   (:foreground ,string)
   nil)
  (company-tooltip
   (:background ,bg-darker
                :foreground ,gray)
   nil)
  (company-scrollbar-fg
   (:background ,comment)
   nil)
  (company-scrollbar-bg
   (:background ,bg-darker)
   nil)
  (company-tooltip-common
   (:foreground ,keyword)
   nil)
  (company-tootip-annotation
   (:foreground ,type)
   nil)
  (company-tooltip-selection
   (:background ,region)
   nil)
  (show-paren-match
   (:background ,keyword
                :foreground ,bg-dark)
   nil)
  (magit-section-heading
   (:foreground ,keyword)
   nil)
  (magit-header-line
   (:background nil
                :foreground ,bg-dark
                :box nil)
   (:background nil
                :foreground ,bg-white
                :box nil))
  (magit-diff-hunk-heading
   (:background ,comment
                :foreground ,gray)
   nil)
  (magit-diff-hunk-heading-highlight
   (:background ,comment
                :foreground ,fg-white)
   nil)
  (tooltip
   (:foreground ,gray
                :background ,bg-darker)
   nil)
  (git-gutter-fr:modified
   (:foreground ,dark-cyan)
   nil)
  (doom-neotree-dir-face
   (:foreground ,keyword
                :height 1.0)
   (:family ,sans-font
            :height 1.0))
  (doom-neotree-file-face
   (:height 1.0)
   (:family ,sans-font
            :height 1.0))
  (doom-neotree-text-file-face
   (:height 1.0)
   (:family ,sans-font
            :height 1.0))
  (doom-neotree-hidden-file-face
   (:height 1.0
            :foreground ,comment)
   (:family ,sans-font
            :height 1.0
            :foreground ,comment))
  (doom-neotree-media-file-face
   (:height 1.0
            :foreground ,type)
   (:family ,sans-font
            :height 1.0
            :foreground ,type))
  (doom-neotree-data-file-face
   (:height 1.0
            :foreground ,doc)
   (:family ,sans-font
            :height 1.0
            :foreground ,doc))
  (neo-root-dir-face
   (:foreground ,fg-white
                :background ,region-dark
                :box (:line-width 6 :color ,region-dark))
   nil)
  (mode-line
   (:background ,bg-darker)
   (:background ,bg-white
                :box nil))
  (mode-line-inactive
   nil
   (:box nil))
  (powerline-active1
   nil
   (:background ,bg-white))
  (powerline-active2
   nil
   (:background ,bg-white))
  (powerline-inactive1
   nil
   (:background ,bg-white))
  (powerline-inactive2
   nil
   (:background ,bg-white))
  (highlight
   (:background ,region
                :foreground ,fg-white)
   (:background ,shade-white))
  (hl-line
   (:background ,region-dark)
   nil)
  (solaire-hl-line-face
   (:background ,bg-dark)
   nil)
  (org-document-title
   (:inherit variable-pitch
             :height 1.3
             :weight normal
             :foreground ,gray)
   (:inherit nil
             :family ,et-font
             :height 1.8
             :foreground ,bg-dark
             :underline nil))
  (org-document-info
   (:foreground ,gray
                :slant italic)
   (:height 1.2
            :slant italic))
  (org-level-1
   (:inherit variable-pitch
             :height 1.3
             :weight bold
             :foreground ,keyword
             :background ,bg-dark)
   (:inherit nil
             :family ,et-font
             :height 1.6
             :weight normal
             :slant normal
             :foreground ,bg-dark))
  (org-level-2
   (:inherit variable-pitch
             :weight bold
             :height 1.2
             :foreground ,gray
             :background ,bg-dark)
   (:inherit nil
             :family ,et-font
             :weight normal
             :height 1.3
             :slant italic
             :foreground ,bg-dark))
  (org-level-3
   (:inherit variable-pitch
             :weight bold
             :height 1.1
             :foreground ,slate
             :background ,bg-dark)
   (:inherit nil
             :family ,et-font
             :weight normal
             :slant italic
             :height 1.2
             :foreground ,bg-dark))
  (org-level-4
   (:inherit variable-pitch
             :weight bold
             :height 1.1
             :foreground ,slate
             :background ,bg-dark)
   (:inherit nil
             :family ,et-font
             :weight normal
             :slant italic
             :height 1.1
             :foreground ,bg-dark))
  (org-level-5
   (:inherit variable-pitch
             :weight bold
             :height 1.1
             :foreground ,slate
             :background ,bg-dark)
   nil)
  (org-level-6
   (:inherit variable-pitch
             :weight bold
             :height 1.1
             :foreground ,slate
             :background ,bg-dark)
   nil)
  (org-level-7
   (:inherit variable-pitch
             :weight bold
             :height 1.1
             :foreground ,slate
             :background ,bg-dark)
   nil)
  (org-level-8
   (:inherit variable-pitch
             :weight bold
             :height 1.1
             :foreground ,slate
             :background ,bg-dark)
   nil)
  (org-headline-done
   (:strike-through t)
   (:family ,et-font
            :strike-through t))
  (org-quote
   (:background ,bg-dark)
   nil)
  (org-block
   (:background ,bg-dark)
   (:background nil
                :foreground ,bg-dark))
  (org-block-begin-line
   (:background ,bg-dark)
   (:background nil
                :height 0.8
                :family ,sans-mono-font
                :foreground ,slate))
  (org-block-end-line
   (:background ,bg-dark)
   (:background nil
                :height 0.8
                :family ,sans-mono-font
                :foreground ,slate))
  (org-document-info-keyword
   (:foreground ,comment)
   (:height 0.8
            :foreground ,gray))
  (org-link
   (:underline nil
               :weight normal
               :foreground ,slate)
   (:foreground ,bg-dark))
  (org-special-keyword
   (:height 0.9
            :foreground ,comment)
   (:family ,sans-mono-font
            :height 0.8))
  (org-todo
   (:foreground ,builtin
                :background ,bg-dark)
   nil)
  (org-done
   (:inherit variable-pitch
             :foreground ,dark-cyan
             :background ,bg-dark)
   nil)
  (org-agenda-current-time
   (:foreground ,slate)
   nil)
  (org-hide
   nil
   (:foreground ,bg-white))
  (org-indent
   (:inherit org-hide)
   (:inherit (org-hide fixed-pitch)))
  (org-time-grid
   (:foreground ,comment)
   nil)
  (org-warning
   (:foreground ,builtin)
   nil)
  (org-date
   nil
   (:family ,sans-mono-font
            :height 0.8))
  (org-agenda-structure
   (:height 1.3
            :foreground ,doc
            :weight normal
            :inherit variable-pitch)
   nil)
  (org-agenda-date
   (:foreground ,doc
                :inherit variable-pitch)
   (:inherit variable-pitch
             :height 1.1))
  (org-agenda-date-today
   (:height 1.5
            :foreground ,keyword
            :inherit variable-pitch)
   nil)
  (org-agenda-date-weekend
   (:inherit org-agenda-date)
   nil)
  (org-scheduled
   (:foreground ,gray)
   nil)
  (org-upcoming-deadline
   (:foreground ,keyword)
   nil)
  (org-scheduled-today
   (:foreground ,fg-white)
   nil)
  (org-scheduled-previously
   (:foreground ,slate)
   nil)
  (org-agenda-done
   (:inherit nil
             :strike-through t
             :foreground ,doc)
   (:strike-through t
                    :foreground ,doc))
  (org-ellipsis
   (:underline nil
               :foreground ,comment)
   (:underline nil
               :foreground ,comment))
  (org-tag
   (:foreground ,doc)
   (:foreground ,doc))
  (org-table
   (:background nil)
   (:family ,serif-mono-font
            :height 0.9
            :background ,bg-white))
  (org-code
   (:inherit font-lock-builtin-face)
   (:inherit nil
             :family ,serif-mono-font
             :foreground ,comment
             :height 0.9))
  (font-latex-sectioning-0-face
   (:foreground ,type
                :height 1.2)
   nil)
  (font-latex-sectioning-1-face
   (:foreground ,type
                :height 1.1)
   nil)
  (font-latex-sectioning-2-face
   (:foreground ,type
                :height 1.1)
   nil)
  (font-latex-sectioning-3-face
   (:foreground ,type
                :height 1.0)
   nil)
  (font-latex-sectioning-4-face
   (:foreground ,type
                :height 1.0)
   nil)
  (font-latex-sectioning-5-face
   (:foreground ,type
                :height 1.0)
   nil)
  (font-latex-verbatim-face
   (:foreground ,builtin)
   nil)
  (spacemacs-normal-face
   (:background ,bg-dark
                :foreground ,fg-white)
   nil)
  (spacemacs-evilified-face
   (:background ,bg-dark
                :foreground ,fg-white)
   nil)
  (spacemacs-lisp-face
   (:background ,bg-dark
                :foreground ,fg-white)
   nil)
  (spacemacs-emacs-face
   (:background ,bg-dark
                :foreground ,fg-white)
   nil)
  (spacemacs-motion-face
   (:background ,bg-dark
                :foreground ,fg-white)
   nil)
  (spacemacs-visual-face
   (:background ,bg-dark
                :foreground ,fg-white)
   nil)
  (spacemacs-hybrid-face
   (:background ,bg-dark
                :foreground ,fg-white)
   nil)
  (bm-persistent-face
   (:background ,dark-cyan
                :foreground ,fg-white)
   nil)
  (helm-selection
   (:background ,region)
   nil)
  (helm-match
   (:foreground ,keyword)
   nil)
  (cfw:face-title
   (:height 2.0
            :inherit variable-pitch
            :weight bold
            :foreground ,doc)
   nil)
  (cfw:face-holiday
   (:foreground ,builtin)
   nil)
  (cfw:face-saturday
   (:foreground ,doc
                :weight bold)
   nil)
  (cfw:face-sunday
   (:foreground ,doc)
   nil)
  (cfw:face-periods
   (:foreground ,dark-cyan)
   nil)
  (cfw:face-annotation
   (:foreground ,doc)
   nil)
  (cfw:face-select
   (:background ,region)
   nil)
  (cfw:face-toolbar-button-off
   (:foreground ,doc)
   nil)
  (cfw:face-toolbar-button-on
   (:foreground ,type
                :weight bold)
   nil)
  (cfw:face-day-title
   (:foreground ,doc)
   nil)
  (cfw:face-default-content
   (:foreground ,dark-cyan)
   nil)
  (cfw:face-disable
   (:foreground ,doc)
   nil)
  (cfw:face-today
   (:background ,region
                :weight bold)
   nil)
  (cfw:face-toolbar
   (:inherit default)
   nil)
  (cfw:face-today-title
   (:background ,keyword
                :foreground ,fg-white)
   nil)
  (cfw:face-grid
   (:foreground ,comment)
   nil)
  (cfw:face-header
   (:foreground ,keyword
                :weight bold)
   nil)
  (cfw:face-default-day
   (:foreground ,fg-white)
   nil)
  (dired-subtree-depth-1-face
   (:background nil)
   nil)
  (dired-subtree-depth-2-face
   (:background nil)
   nil)
  (dired-subtree-depth-3-face
   (:background nil)
   nil)
  (dired-subtree-depth-4-face
   (:background nil)
   nil)
  (dired-subtree-depth-5-face
   (:background nil)
   nil)
  (dired-subtree-depth-6-face
   (:background nil)
   nil)
  (nlinum-current-line
   (:foreground ,builtin)
   (:foreground ,bg-dark))
  (vertical-border
   (:background ,region
                :foreground ,region)
   nil)
  (which-key-command-description-face
   (:foreground ,type)
   nil)
  (flycheck-error
   (:background nil)
   nil)
  (flycheck-warning
   (:background nil)
   nil)
  (font-lock-string-face
   (:foreground ,string)
   nil)
  (font-lock-comment-face
   (:foreground ,doc
                :slant italic)
   (:background nil
                :foreground ,doc
                :slant italic))
  (helm-ff-symlink
   (:foreground ,slate)
   nil)
  (region
   (:background ,region)
   nil)
  (header-line
   (:background nil
                :inherit nil)
   (:background nil
                :inherit nil))))


(let* ((variable-tuple
        (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
              ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
              ((x-list-fonts "Verdana")         '(:font "Verdana"))
              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  (custom-theme-set-faces
   'user
   `(org-level-8 ((t (,@headline ,@variable-tuple))))
   `(org-level-7 ((t (,@headline ,@variable-tuple))))
   `(org-level-6 ((t (,@headline ,@variable-tuple))))
   `(org-level-5 ((t (,@headline ,@variable-tuple))))
   `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
   `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
   `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
   `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
   `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "ETBembo" :height 180 :weight thin))))
 ;; '(fixed-pitch ((t ( :family "Fira Code Retina" :height 160)))))
 '(fixed-pitch ((t ( :family "Bront" :height 160)))))

(setq org-hide-emphasis-markers t)

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))


(custom-theme-set-faces
 'user
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

  )
; #################################################################################################################














;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check https://github.com/rougier/elegant-emacs/issues/4 for this ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (set-face-font 'default "Roboto Mono Light 14")

;; (set-frame-parameter (selected-frame)
;;                      'internal-border-width 24)
;; (setq default-frame-alist
;;       (append (list '(width  . 72) '(height . 40)
;;                     '(vertical-scroll-bars . nil)
;;                     '(internal-border-width . 24)
;;                     '(font . "Roboto Mono Light 14"))))


;; ;; Line spacing, can be 0 for code and 1 or 2 for text
;; (setq-default line-spacing 0)

;; ;; Underline line at descent position, not baseline position
;; (setq x-underline-at-descent-line t)

;; ;; No ugly button for checkboxes
;; (setq widget-image-enable nil)

;; ;; Line cursor and no blink
;; (set-default 'cursor-type  '(bar . 1))
;; (blink-cursor-mode 0)

;; ;; No sound
;; (setq visible-bell t)
;; (setq ring-bell-function 'ignore)

;; ;; Paren mode is part of the theme
;; (show-paren-mode t)

;; ;; this is a purposefully long line that I hope will show some things in the fringe
;; (fringe-mode '(0 . 0))
;; (defface fallback '((t :family "Fira Code Light"
;;                        :inherit 'face-faded)) "Fallback")
;; (set-display-table-slot standard-display-table 'truncation
;;                         (make-glyph-code ?… 'fallback))
;; (set-display-table-slot standard-display-table 'wrap
;;                         (make-glyph-code ?↩ 'fallback))

;; ;; simplified mode line
;; (defun mode-line-render (left right)
;;   (let* ((available-width (- (window-width) (length left) )))
;;     (format (format "%%s %%%ds" available-width) left right)))
;; (setq-default mode-line-format
;;      '((:eval
;;        (mode-line-render
;;        (format-mode-line (list
;;          (propertize "☰" 'face `(:inherit mode-line-buffer-id)
;;                          'help-echo "Mode(s) menu"
;;                          'mouse-face 'mode-line-highlight
;;                          'local-map   mode-line-major-mode-keymap)
;;          " %b "
;;          (if (and buffer-file-name (buffer-modified-p))
;;              (propertize "(modified)" 'face `(:inherit face-faded)))))
;;        (format-mode-line
;;         (propertize "%4l:%2c  " 'face `(:inherit face-faded)))))))

;; ;; move modeline to the top of the buffer
;; (setq-default header-line-format mode-line-format)
;; (setq-default mode-line-format'(""))

;; ;; Vertical window divider
;; (setq window-divider-default-right-width 3)
;; (setq window-divider-default-places 'right-only)
;; (window-divider-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check https://lepisma.xyz/2017/10/28/ricing-org-mode/ for this ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
