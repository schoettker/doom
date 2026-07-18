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

(defun lschoettker/sp-new (name &optional branch)
  "Create a services-pilot worktree NAME via sp-new and switch to it.
Runs sp-new asynchronously; on success adds the worktree to projectile,
switches to a workspace named NAME, and opens magit-status there. The spt
bookkeeping keeps running in the background (see /tmp/sp-new-NAME.spt.log).
With prefix arg, also prompt for BRANCH (sp-new defaults it to NAME)."
  (interactive
   (let ((name (string-trim (read-string "Worktree name: "))))
     (list name
           (when current-prefix-arg
             (read-string (format "Branch (default %s): " name)
                          nil nil name)))))
  (when (string-empty-p name)
    (user-error "Worktree name required"))
  (let ((target (expand-file-name name lschoettker/sp-root))
        (buffer (get-buffer-create (format "*sp-new: %s*" name))))
    (when (file-directory-p target)
      (user-error "Worktree already exists: %s" target))
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
           (projectile-add-known-project (file-name-as-directory target))
           (+workspace-switch name t)
           (magit-status target)
           (message "sp-new %s: ready (spt bookkeeping in background)"
                    name)))))))

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
