;;; ~/.doom.d/+keybindings.el -*- lexical-binding: t; -*-

;; Leader bindings
(map! :leader
      (:n ":" #'doom/open-scratch-buffer)
      (:n "x" #'counsel-M-x)

      (:desc "prev buffer" :n "TAB" #'mode-line-other-buffer) ;; maybe evil-switch-to-window-last-buffer?!
      (:desc "go to char" :n "SPC" #'avy-goto-word-or-subword-1)
      (:desc "add ;" :n "RET" #'add-semicolon-at-eol)
      (:desc "swiper" :n "/" #'counsel-grep-or-swiper)
      (:desc "kill buffer" :n "d" #'kill-this-buffer)

      (:desc "apps" :prefix "a"
        :desc "ag" :n "a" #'ag
        :desc "dired" :n "d" #'dired
        :desc "termite" :n "t" #'open-termite)

      (:desc "buffer" :prefix "b"
        ;; :desc "agenda buffer" :n "a" #'(lambda () (interactive) (switch-to-buffer "*Org Agenda*"))
        :desc "counsel boomarks" :n "j" #'counsel-bookmark
        :desc "list bookmarks" :n "l" #'list-bookmarks)

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
        :desc "agenda" :n "a" #'org-agenda-list
        "A" nil
        (:prefix ("A" . "org agenda")
          :desc "Agenda"         "a"  #'org-agenda
          :desc "Todo list"      "t"  #'org-todo-list
          :desc "Tags search"    "m"  #'org-tags-view
          :desc "View search"    "v"  #'org-search-view
          )
        :desc "agenda headings" :n "h" #'counsel-org-agenda-headlines
        :desc "todo's" :n "t" #'org-todo-list
        :desc "open config" :n "c" #'(lambda () (interactive) (find-file "~/.doom.d/config.el")))

      (:desc "window" :prefix "w"
        :desc "close workspace" :n "D" #'eyebrowse-close-window-config
        :desc "maximize buffer" :n "m" #'doom/window-maximize-buffer)

      (:desc "z" :prefix "z"
        :desc "narrow" :n "n" #'narrow-to-region
        :desc "widen" :n "w" #'widen))

;; Normal/visual mode bindings
(map! :nv "C-c a" #'my-increment-number-at-point)
(map! :nv "C-c x" #'my-decrement-number-at-point)
(map! :nv "j" #'evil-next-visual-line)
(map! :nv "k" #'evil-previous-visual-line)
(map! :nvime "C-y" #'yank)
(map! :nvme "\\" #'evil-ex-nohighlight)
