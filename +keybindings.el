;;; +keybindings.el -*- lexical-binding: t; -*-

;; Free <leader>a prefix
(map!
        (:map minibuffer-local-map
        (:leader
         :desc "apps" "a" nil)))

;; Leader bindings
(map! :leader
      (:n ":" #'doom/open-scratch-buffer)
      (:n "x" #'execute-extended-command)

      (:desc "prev buffer" :n "TAB" #'mode-line-other-buffer) ;; maybe evil-switch-to-window-last-buffer?!
      (:desc "go to char" :n "SPC" #'avy-goto-word-or-subword-1)
      (:desc "swiper" :n "/" #'+default/search-buffer)
      (:desc "kill buffer" :n "d" #'kill-this-buffer)

      (:desc "apps" :prefix "a"
        :desc "ag" :n "a" #'ag
        :desc "dired" :n "d" #'dired
        :desc "eshell" :n "e" #'+eshell/toggle
        )


      (:desc "e" :prefix "e"
        :desc "next error" :n "n" #'flycheck-next-error
        :desc "prev error" :n "p" #'flycheck-previous-error
        )

      (:desc "+file" :prefix "f"
        :desc "find other window" :n "o" #'find-file-other-window
       )

      (:desc "git" :prefix "g"
        :desc "file diff" :n "d" #'magit-diff-buffer-file
        :desc "buffer log" :n "l" #'magit-log-buffer-file
        :desc "status" :n "s" #'magit-status)

      (:desc "Project" :prefix "p"
        :desc "search in project" :n "/" #'+default/search-project
        :desc "search from current dir" :n "." #'+default/search-cwd
        :desc "find file" :n "p" #'projectile-find-file
        :desc "find file" :n "f" #'projectile-find-file
        :desc "switch project" :n "s" #'projectile-switch-project)

      (:desc "window" :prefix "w"
       :desc "add window config to register" :n "a" #'window-configuration-to-register
       :desc "maximize buffer" :n "m" #'doom/window-maximize-buffer)

      (:desc "yank ring" :n "y" #'browse-kill-ring)

      (:desc "z" :prefix "z"
        :desc "narrow" :n "n" #'narrow-to-region
        :desc "widen" :n "w" #'widen))

;; Winum keybidings for switching windows
(after! winum
  (map! :leader
        (:desc "win 1" :n "1" #'winum-select-window-1)
        (:desc "win 2" :n "2" #'winum-select-window-2)
        (:desc "win 3" :n "3" #'winum-select-window-3)
        (:desc "win 4" :n "4" #'winum-select-window-4)
        (:desc "win 5" :n "5" #'winum-select-window-5)))


;; Normal/visual mode bindings
(map! :nv "g[" #'git-gutter:previous-hunk)
(map! :nv "g]" #'git-gutter:next-hunk)


;; Export current canidates/results (like ivy-occur)
(map! (:map minibuffer-local-map
      ;; (:leader
      ;;  "A" #'embark-dwim)
       "C-c C-o" #'embark-export))
