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
