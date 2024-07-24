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

;; (defun url-escape-string (str)
;;   "Escape special characters in STR for use in a URL."
;;   (replace-regexp-in-string
;;    "[^A-Za-z0-9_~.\\-]"
;;    (lambda (match)
;;      (format "%%%02x" (string-to-char match)))
;;    str))

;; (defun create-obsidian-uri ()
;;   "Create an Obsidian URI string with current file path and line number."
;;   (let* ((vault "dizzy")
;;          (file-path (url-escape-string (current-file-relative-path)))
;;          (line-number (current-line-number)))
;;     (format "obsidian://advanced-uri?vault=%s&filepath=%s&line=%d"
;;             vault file-path line-number)))

(defun create-obsidian-uri ()
  "Create an Obsidian URI string with current file path and line number."
  (let* ((vault "dizzy")
         (file-path (url-encode-url (current-file-relative-path)))
         (line-number (current-line-number)))
    (format "obsidian://advanced-uri?vault=%s&filepath=%s&line=%d"
            vault file-path line-number)))


(defun show-obsidian-uri ()
  "Show the Obsidian URI string in the minibuffer."
  (interactive)
  (let ((uri (create-obsidian-uri)))
    (message "Obsidian URI: %s" uri)))

;; (defun open-in-obsidian ()
;;   "Open Obsidian with the generated Obsidian URI."
;;   (interactive)
;;   (let ((obsidian-app "/Applications/Obsidian.app")
;;         (uri (create-obsidian-uri)))
;;     (start-process "obsidian" nil "open" "-b" obsidian-app "--args" "--background" uri)))

(defun open-in-obsidian ()
  "Open Obsidian with the generated Obsidian URI."
  (interactive)
  (let ((obsidian-app "/Applications/Obsidian.app")
        (uri (create-obsidian-uri)))
    (shell-command (format "open -a %s --background '%s'" obsidian-app uri))))

;; { "-a", "/Applications/Obsidian.app", "--background", "'obsidian://advanced-uri?vault=dizzy&filepath=%F0%9F%97%93%EF%B8%8F%20Journal%2Fweekly%20F2024%2F01-January%2F2024-W02.md&line=39'"


;; Emacs Obsidian URI: obsidian://advanced-uri?vault=dizzy&filepath=%1f5d3%fe0f%20Journal%2fdaily%2f2024%2f01-January%2f2024-01-30%20Tuesday.md&line=21


;; "'obsidian://advanced-uri?vault=dizzy&filepath=%F0%9F%97%93%EF%B8%8F%20Journal%2Fweekly%2 F2024%2F01-January%2F2024-W02.md&line=39'"


;; obsidian://advanced-uri?vault=dizzy&filepath=%F0%9F%97%93%EF%B8%8F%20Journal%2Fweekly%20F2024%2F01-January%2F2024-W02.md&line=39
;; obsidian://advanced-uri?vault=dizzy&filepath=%1f5d3%fe0f%20Journal%2fdaily%2f2024%2f01-January%2f2024-01-30%20Tuesday.md&line=21
;; obsidian://advanced-uri?vault=dizzy&filepath=%1f5d3%fe0f%20Journal%2fweekly%2f2024%2f01-January%2f2024-W02.md&line=1
