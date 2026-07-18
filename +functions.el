;;; +functions.el -*- lexical-binding: t; -*-

(defun git-root ()
  "Find the root directory of the current git project."
  (let ((default-directory (or (locate-dominating-file default-directory ".git")
                               default-directory)))
    (when default-directory
      (expand-file-name default-directory))))

(defun current-file-relative-path ()
  "Return the relative path of the current buffer's file from the git root directory."
  (let* ((file-path (buffer-file-name))
         (git-root (git-root)))
    (when (and file-path git-root)
      (file-relative-name file-path git-root))))

(defun current-line-number ()
  "Return the current line number in the buffer."
  (line-number-at-pos))

(defun lschoettker/magit-pr-diff ()
  "Show diff of current branch against origin/master (PR view)."
  (interactive)
  (magit-diff-range (concat "origin/master..." (magit-get-current-branch))
                    '("--stat")))

(defun lschoettker/magit-pr-log ()
  "Show commit log of current branch against origin/master."
  (interactive)
  (magit-log-other (list (concat "origin/master.." (magit-get-current-branch)))))

(defvar lschoettker/work-projects
  '(("dynamic-user-reporting-forms"
     :path "~/work/dynamic-user-reporting-forms"
     :dev-cmd "pnpm dev"
     :alt-cmd "pnpm env use 22 --global")
    ("support-site-frontend"
     :path "~/work/support-site-frontend"
     :dev-cmd "make start"
     :alt-cmd "pnpm env use 16 --global")
    ("tintin"
     :path "~/work/tintin"
     :dev-cmd "gcloud beta emulators pubsub start --project=test-project"
     :alt-cmd "make test")
    ("cheerleader"
     :path "~/work/cheerleader"
     :dev-cmd "cloud_sql_proxy -enable_iam_login -instances=toolbox-api:europe-west1:user-reporting=tcp:5432 --token=$(gcloud auth print-access-token --impersonate-service-account=cheerleader@gke-accounts.iam.gserviceaccount.com)"
     :alt-cmd "# Run tests or other commands"))
  "List of work projects with their paths and commands.
Each project is a list with name followed by plist of :path, :dev-cmd, :alt-cmd.")

(defun lschoettker/setup-project-layout (project-config)
  "Set up the standard layout for a single project.
PROJECT-CONFIG should be an entry from `lschoettker/work-projects'."
  (let* ((project-name (car project-config))
         (project-plist (cdr project-config))
         (project-path (plist-get project-plist :path))
         (dev-cmd (plist-get project-plist :dev-cmd))
         (alt-cmd (plist-get project-plist :alt-cmd))
         (expanded-path (expand-file-name project-path)))
    (unless (file-directory-p expanded-path)
      (user-error "Project directory does not exist: %s" expanded-path))

    ;; Switch to project without prompting for a file
    (let ((default-directory expanded-path))
      (projectile-add-known-project expanded-path))

    ;; Clear current layout and ensure we start from the left window
    (delete-other-windows)
    (select-window (frame-first-window))

    ;; Create vertical split (left for magit, right for terminals)
    (let ((left-window (selected-window))
          (right-window (split-window-right)))

      ;; Left side: magit status (ensure we're in left window)
      (select-window left-window)
      (magit-status expanded-path)

      ;; Right side: split horizontally for two terminals
      (select-window right-window)
      (let ((top-right-window (selected-window))
            (bottom-right-window (split-window-below)))

        ;; Top right terminal with dev command
        (select-window top-right-window)
        (let ((default-directory expanded-path))
          (+vterm/here nil)
          (when dev-cmd
            (let ((vterm-buffer (current-buffer)))
              (run-at-time 0.5 nil
                           (lambda ()
                             (with-current-buffer vterm-buffer
                               (when (get-buffer-process vterm-buffer)
                                 (vterm-send-string dev-cmd))))))))

        ;; Bottom right terminal with alt command
        (select-window bottom-right-window)
        (let ((default-directory expanded-path))
          (+vterm/here nil)
          (when alt-cmd
            (let ((vterm-buffer (current-buffer)))
              (run-at-time 0.5 nil
                           (lambda ()
                             (with-current-buffer vterm-buffer
                               (when (get-buffer-process vterm-buffer)
                                 (vterm-send-string alt-cmd))))))))

        ;; Return focus to magit
        (select-window left-window)))))

(defun lschoettker/setup-all-work-projects ()
  "Set up workspaces for all work projects using tabs/workspaces."
  (interactive)
  (dolist (project lschoettker/work-projects)
    (let ((project-name (car project)))
      ;; Create or switch to workspace for this project
      (+workspace-new project-name)
      (+workspace-switch project-name)
      (lschoettker/setup-project-layout project)))

  (message "All work project workspaces have been set up"))

(defun lschoettker/setup-single-work-project (project-name)
  "Set up workspace for a single work project.
PROJECT-NAME should be one of the keys from `lschoettker/work-projects'."
  (interactive
   (list (completing-read "Choose work project: "
                          (mapcar #'car lschoettker/work-projects))))
  (let ((project-config (assoc project-name lschoettker/work-projects)))
    (unless project-config
      (user-error "Project not found: %s" project-name))
    (lschoettker/setup-project-layout project-config)
    (message "Work project workspace setup complete for %s" project-name)))

;;; services-pilot worktrees (sp-new / sp-rm) --------------------------------

(defvar lschoettker/sp-root "~/src/services-pilot"
  "Root directory holding services-pilot worktrees.")

(defun lschoettker/sp--worktrees ()
  "Names of existing worktrees under `lschoettker/sp-root', excluding master."
  (let ((root (expand-file-name lschoettker/sp-root)))
    (delete "master"
            (mapcar #'file-name-nondirectory
                    (seq-filter #'file-directory-p
                                (directory-files root t "^[^.]"))))))

(defun lschoettker/sp--script (name)
  (or (executable-find name)
      (expand-file-name name "~/.local/bin")))

(defun lschoettker/sp--switch-to (name target)
  "Switch to worktree TARGET in a workspace named NAME with a ghostel terminal."
  (projectile-add-known-project (file-name-as-directory target))
  (+workspace-switch name t)
  (let ((default-directory (file-name-as-directory target)))
    (+ghostel/here)))

(defun lschoettker/sp-new (name &optional branch)
  "Create or switch to a services-pilot worktree NAME.
If the worktree already exists, just switch to its workspace and open ghostel.
Otherwise run sp-new asynchronously to create it. The spt bookkeeping keeps
running in the background (/tmp/sp-new-NAME.spt.log).
With prefix arg, also prompt for BRANCH (sp-new defaults it to NAME)."
  (interactive
   (let* ((existing (lschoettker/sp--worktrees))
          (name (string-trim
                 (completing-read "Worktree (or new name): " existing nil nil))))
     (list name
           (when (and current-prefix-arg (not (member name existing)))
             (read-string (format "Branch (default %s): " name)
                          nil nil name)))))
  (when (string-empty-p name)
    (user-error "Worktree name required"))
  (let ((target (expand-file-name name lschoettker/sp-root)))
    (if (file-directory-p target)
        (progn
          (lschoettker/sp--switch-to name target)
          (message "Switched to worktree %s" name))
      (let ((buffer (get-buffer-create (format "*sp-new: %s*" name))))
        (with-current-buffer buffer
          (let ((inhibit-read-only t)) (erase-buffer)))
        (message "sp-new %s: creating worktree..." name)
        (make-process
         :name (concat "sp-new-" name)
         :buffer buffer
         :command (delq nil (list (lschoettker/sp--script "sp-new") name branch))
         :sentinel
         (lambda (proc _event)
           (when (memq (process-status proc) '(exit signal))
             (if (/= (process-exit-status proc) 0)
                 (progn
                   (pop-to-buffer (process-buffer proc))
                   (message "sp-new %s failed - see %s" name (buffer-name)))
               (lschoettker/sp--switch-to name target)
               (message "sp-new %s: ready (spt bookkeeping in background)"
                        name)))))))))

(defun lschoettker/sp-rm (name &optional force)
  "Remove services-pilot worktree NAME via sp-rm.
With prefix arg, pass -f (force removal of a dirty worktree). Also deletes
the matching workspace and forgets the projectile project."
  (interactive
   (list (completing-read "Remove worktree: " (lschoettker/sp--worktrees)
                          nil t)
         current-prefix-arg))
  (let ((target (expand-file-name name lschoettker/sp-root)))
    (unless (yes-or-no-p (format "Remove worktree %s and its branch? " name))
      (user-error "Aborted"))
    (with-temp-buffer
      (let ((status (apply #'call-process (lschoettker/sp--script "sp-rm")
                           nil t nil
                           (delq nil (list name (when force "-f"))))))
        (if (/= status 0)
            (error "sp-rm failed: %s" (string-trim (buffer-string)))
          (when (equal (+workspace-current-name) name)
            (+workspace-switch +workspaces-main t))
          (when (+workspace-exists-p name)
            (+workspace-delete name))
          (projectile-remove-known-project (file-name-as-directory target))
          (message "%s" (string-trim (buffer-string))))))))

(defvar-local lschoettker/sp--compile-worktree nil
  "Worktree root of a bazel build started by `lschoettker/sp-compile'.")

(defun lschoettker/sp--default-bazel-target ()
  "Guess a bazel target pattern from the current buffer's path."
  (when-let* ((root (doom-project-root))
              (file (or buffer-file-name default-directory))
              (rel (file-relative-name file root)))
    (unless (string-prefix-p ".." rel)
      (let ((parts (split-string rel "/" t)))
        (when (>= (length parts) 2)
          (format "//%s/%s/..." (nth 0 parts) (nth 1 parts)))))))

(defun lschoettker/sp-compile (target)
  "Run bazel build TARGET in the current project via `compilation-mode'.
If the build fails on missing packages (Tier-1 sparse checkout), offers to
run \"spt git:sparse resolve-dependencies\" to escalate the worktree to
Tier 2, then re-run the build."
  (interactive
   (list (read-string "Bazel target: " (lschoettker/sp--default-bazel-target))))
  (let ((default-directory (or (doom-project-root) default-directory))
        (compilation-buffer-name-function
         (lambda (&rest _) (format "*bazel: %s*" (+workspace-current-name)))))
    (with-current-buffer (compile (format "bazel build %s" target))
      (setq lschoettker/sp--compile-worktree default-directory))))

(defun lschoettker/sp--offer-resolve-deps (buffer status)
  "Offer Tier-2 dep resolution when a sp-compile BUFFER failed on packages.
STATUS is the compilation exit description."
  (when (and (buffer-local-value 'lschoettker/sp--compile-worktree buffer)
             (not (string-match-p "finished" status))
             (with-current-buffer buffer
               (save-excursion
                 (goto-char (point-min))
                 (re-search-forward "no such package" nil t))))
    (if (y-or-n-p "Missing packages (Tier-1 checkout) - run spt git:sparse resolve-dependencies? ")
        (let ((default-directory
               (buffer-local-value 'lschoettker/sp--compile-worktree buffer)))
          (compile "spt git:sparse resolve-dependencies"))
      (message "Tip: add single dirs with spt git:sparse add --no-resolve-deps <dir>"))))

(add-to-list 'compilation-finish-functions #'lschoettker/sp--offer-resolve-deps)
