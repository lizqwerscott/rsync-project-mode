;;; rsync-project-mode.el --- Rsync project to remote machines  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; URL: https://github.com/lizqwerscott/rsync-project-mode
;; Version: 0.1.0
;; Keywords: rsync, project

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'f)
(require 's)
(require 'transient)

(defgroup rsync-project nil
  "Convenient project remote synchronization."
  :group 'convenience
  :prefix "rsync-project-"
  :link '(url-link "https://github.com/lizqwerscott/rsync-project-mode"))

(defcustom rsync-project-default-auto-rsyncp nil
  "Whether to automatically sync in the background when creating a new remote project."
  :group 'rsync-project
  :type 'boolean)

(defcustom rsync-project-default-gitignorep nil
  "Whether to read the contents of `.gitignore' as filter content when creating a new remote project."
  :group 'rsync-project
  :type 'boolean)

(defcustom rsync-project-list-file (if (boundp 'no-littering-var-directory )
                                       (f-join no-littering-var-directory
                                               "rsync-project-list-file.el")
                                     (f-join user-emacs-directory
                                             "rsync-project-list-file.el"))
  "File in which to save the list of known projects."
  :group 'rsync-project
  :type 'file)

(defvar rsync-project-remote-list nil
  "List of project rsync remote server.")

(defvar-local rsync-project-mode nil
  "Whether rsync-mode is enabled.")

(defvar rsync-project-states (make-hash-table :test #'equal)
  "Rsync project remote states and process")

(defface rsync-project-start-face
  '((t :foreground "green"))
  "Face for 'Start' state.")

(defface rsync-project-stop-face
  '((t :foreground "red"))
  "Face for 'Stop' state.")

(define-minor-mode rsync-project-mode
  "Toggle rsync project mode."
  :init-value nil
  :group 'rsync-project
  (rsync-project-read-list)
  (when-let* ((project-root (project-current))
              (remote-config (rsync-project-get-remote-config (rsync-project--get-now-project-path))))
    (let ((remote-state (gethash (rsync-project--get-now-project-path)
                                 rsync-project-states)))
      (unless remote-state
        (let ((connectp (rsync-project--test-connection remote-config)))
          (unless connectp
            (message "%s remote can't connect"
                     (plist-get (plist-get remote-config :ssh-config)
                                :host)))
          (puthash (rsync-project--get-now-project-path)
                   (list :connectp connectp
                         :process nil)
                   rsync-project-states)))
      (if (not rsync-project-mode)
          (rsync-project-auto-sync-stop remote-config)
        (when (plist-get remote-config :auto-rsyncp)
          (rsync-project-auto-sync-start remote-config))))))

(defun rsync-project-write-list ()
  "Save the rsync project remote list."
  (let ((filename rsync-project-list-file))
    (with-temp-buffer
      (insert ";;; -*- lisp-data -*-\n")
      (let ((print-length nil)
            (print-level nil))
        (pp rsync-project-remote-list
            (current-buffer)))
      (write-region nil nil filename nil 'silent))))

(defun rsync-project-read-list ()
  "Load the rsync project remote list."
  (let ((filename rsync-project-list-file))
    (setq rsync-project-remote-list
          (when (file-exists-p filename)
            (with-temp-buffer
              (insert-file-contents filename)
              (read (current-buffer)))))
    (unless (seq-every-p
             (lambda (elt) (stringp (cl-getf elt :root-path)))
             rsync-project-remote-list)
      (warn "Contents of %s are in wrong format, resetting"
            rsync-project-list-file)
      (setq rsync-project-remote-list nil))))

(defmacro rsync-project-with-update-list (remote-config-name &rest body)
  "Execute BODY after REMOTE-CONFIG-NAME find in all remote list."
  (declare (indent 1) (debug (form def-body)))
  `(progn
     (rsync-project-read-list)
     (setf rsync-project-remote-list
           (mapcar (lambda (,remote-config-name)
                     (if (string= (cl-getf ,remote-config-name :root-path)
                                  (rsync-project--get-now-project-path))
                         (progn
                           ,@body)
                       ,remote-config-name))
                   rsync-project-remote-list))
     (rsync-project-write-list)))

(defun rsync-project--update-item (plist new-key-values)
  "Update the PLIST with corresponding values from NEW-KEY-VALUES for matching keys."
  (let ((new-plist nil))
    (cl-loop for (key value) on plist by #'cddr
             do (let ((new-value (cl-getf new-key-values key)))
                  (setf new-plist
                        (append new-plist
                                (list key
                                      (if (cl-find key new-key-values)
                                          new-value
                                        value))))))
    new-plist))

(defun rsync-project--get-now-project-path ()
  "Retrieve the absolute path of the current project's root directory."
  (file-truename (project-root (project-current))))

(defun rsync-project-get-remote-config (project-path)
  "Retrieve the remote configuration for the given PROJECT-PATH.
The function searches `rsync-project-remote-list' for a matching
remote configuration based on the truename of PROJECT-PATH."
  (cl-find (file-truename project-path)
           rsync-project-remote-list
           :key #'(lambda (elm)
                    (file-truename
                     (cl-getf elm :root-path)))
           :test #'string=))

(defun rsync-project--test-connection (remote-config)
  "Test connection to the remote host in REMOTE-CONFIG.
REMOTE-CONFIG is a plist containing SSH configuration details,
such as user, host, and port. Returns t if the connection is successful, nil otherwise."
  (let* ((ssh-config (cl-getf remote-config :ssh-config))
         (remote-user (cl-getf ssh-config :user))
         (remote-host (cl-getf ssh-config :host))
         (remote-port (cl-getf ssh-config :port)))
    (= 0
       (shell-command
        ;; (format "ssh %s -o BatchMode=yes -o ConnectTimeout=1 %s true &>/dev/null"
        ;;         (if remote-port
        ;;             (if (not (= 22 remote-port))
        ;;                 (format "-p %s" remote-port)
        ;;               "")
        ;;           "")
        ;;         (if (and remote-user remote-host)
        ;;             (format "%s@%s"
        ;;                     remote-user
        ;;                     remote-host)
        ;;           remote-host))
        (format "ssh -o ConnectTimeout=1 -q %s %s &>/dev/null"
                (if remote-port
                    (if (not (= 22 remote-port))
                        (format "-p %s" remote-port)
                      "")
                  "")
                (if (and remote-user remote-host)
                    (format "%s@%s"
                            remote-user
                            remote-host)
                  remote-host))))))

(defun rsync-project-build-rsync-args (remote-config)
  "Build rsync command arguments based on REMOTE-CONFIG.
REMOTE-CONFIG is a plist containing configuration details such as
local path, SSH configuration, ignore list, and gitignore settings."
  (let* ((local-path (cl-getf remote-config :root-path))
         (ssh-config (cl-getf remote-config :ssh-config))
         (remote-user (cl-getf ssh-config :user))
         (remote-host (cl-getf ssh-config :host))
         (remote-port (cl-getf ssh-config :port))
         (remote-path (cl-getf ssh-config :remote-dir))
         (ignore-list (cl-getf remote-config :ignore-file-list))
         (gitignorep (cl-getf remote-config :gitignorep)))
    `("-avtP"
      ,@(if remote-port
            (if (not (= 22 remote-port))
                `("-e"
                  (format "\"ssh -p %d\"" remote-port))))
      ,@(if gitignorep
            (list "--filter=':- .gitignore'"))
      ,@(mapcar #'(lambda (dir)
                    (concat "--exclude=" dir))
                ignore-list)
      ,local-path
      ,(if (and remote-user remote-host)
           (format "%s@%s:%s"
                   remote-user
                   remote-host
                   remote-path)
         (format "%s:%s"
                 remote-host
                 remote-path)))))

(defun rsync-project-generate-rsync-cmd (remote-config)
  "Generate a rsync command string based on the provided REMOTE-CONFIG.
REMOTE-CONFIG should be a configuration object containing rsync arguments."
  (let ((rsync-args (rsync-project-build-rsync-args remote-config)))
    (apply #'format
           (append (list
                    (concat "rsync "
                            (s-join " "
                                    (make-list (length rsync-args)
                                               "%s"))))
                   rsync-args))))

(defun rsync-project--check ()
  "Project have remote."
  (rsync-project-get-remote-config (rsync-project--get-now-project-path)))

(defun rsync-project--check-auto-rsync ()
  "Check if auto-rsync is enabled for the current project.
This function reads the project list and retrieves the remote configuration
for the current project. If the remote configuration exists, it returns
the value of the `:auto-rsyncp` property."
  (rsync-project-read-list)
  (when-let* ((remote-config (rsync-project-get-remote-config
                              (rsync-project--get-now-project-path))))
    (cl-getf remote-config :auto-rsyncp)))

;;;###autoload
(defun rsync-project-add ()
  "Add now project to rsync list."
  (interactive)
  (let ((project-root-dir (rsync-project--get-now-project-path))
        (name (project-name (project-current))))
    (if (not (rsync-project-get-remote-config project-root-dir))
        (let ((ignore-file-list (list ".git"))
              (remote-dir (tramp-dissect-file-name (read-file-name "Remote dir:" "/ssh:")))
              (add-ignore-filep (yes-or-no-p "Add ignore files?")))
          (let ((remote-dir-path (tramp-file-name-localname remote-dir)))
            (unless (string= (file-name-nondirectory (directory-file-name remote-dir-path))
                             name)
              (setf remote-dir-path
                    (f-join remote-dir-path name)))
            (while add-ignore-filep
              (cl-pushnew (f-filename (read-file-name "Ignore path:" project-root-dir))
                          ignore-file-list)
              (setf add-ignore-filep
                    (yes-or-no-p (format "(%s)Add ignore files?" ignore-file-list))))
            (add-to-list 'rsync-project-remote-list
                         (list :root-path project-root-dir
                               :ssh-config (list :user (tramp-file-name-user remote-dir)
                                                 :host (tramp-file-name-host remote-dir)
                                                 :port (tramp-file-name-port remote-dir)
                                                 :remote-dir remote-dir-path)
                               :ignore-file-list ignore-file-list
                               :gitignorep rsync-project-default-gitignorep
                               :auto-rsyncp rsync-project-default-auto-rsyncp))
            (rsync-project-write-list)))
      (message "Already add now project."))))

;;;###autoload
(defun rsync-project-remove ()
  "Remove now project in rsync list."
  (interactive)
  (rsync-project-read-list)
  (let ((remote-config (rsync-project-get-remote-config (rsync-project--get-now-project-path))))
    (if remote-config
        (progn
          (when (cl-getf remote-config :auto-rsyncp)
            (rsync-project-auto-sync-stop remote-config))
          (setf rsync-project-remote-list
                (cl-remove-if #'(lambda (item)
                                  (string= (cl-getf item :root-path)
                                           (cl-getf remote-config :root-path)))
                              rsync-project-remote-list))
          (rsync-project-write-list))
      (message "Now project not add rsync"))))

;;; handle ignore list
;;;###autoload
(defun rsync-project-add-ignore ()
  "Remove now project in rsync list."
  (interactive)
  (rsync-project-with-update-list remote-config
    (let ((add-ignore-filep t)
          (new-ignore-file-list (cl-getf remote-config :ignore-file-list))
          (project-root-dir (cl-getf remote-config :root-path)))
      (while add-ignore-filep
        (cl-pushnew (f-filename (read-file-name "Ignore path:" project-root-dir))
                    new-ignore-file-list)
        (setf add-ignore-filep
              (yes-or-no-p (format "(%s)Add ignore files?" new-ignore-file-list))))
      (rsync-project--update-item remote-config
                                  (list :ignore-file-list new-ignore-file-list))))
  (call-interactively #'rsync-project-re-auto-rsync))

;;;###autoload
(defun rsync-project-remove-ignore ()
  "Remove now project in rsync list."
  (interactive)
  (rsync-project-with-update-list remote-config
    (let* ((new-ignore-file-list (cl-getf remote-config :ignore-file-list))
           (project-root-dir (cl-getf remote-config :root-path))
           (choice (completing-read "Choose an ignore: " new-ignore-file-list)))
      (rsync-project--update-item remote-config
                                  (list :ignore-file-list
                                        (cl-remove-if #'(lambda (remote-config)
                                                          (string= choice
                                                                   remote-config))
                                                      new-ignore-file-list)))))
  (call-interactively #'rsync-project-re-auto-rsync))

;;;###autoload
(defun rsync-project-sync-all ()
  "Rsync all."
  (interactive)
  (let ((remote-config (rsync-project-get-remote-config (rsync-project--get-now-project-path))))
    (if remote-config
        (let ((rsync-buffer (get-buffer-create "*Rsync project*")))
          (async-shell-command (rsync-project-generate-rsync-cmd remote-config)
                               rsync-buffer))
      (message "Need use add this project"))))

;;; auto sync
(defun rsync-project-auto-sync-start (remote-config)
  "Start the background monitor for REMOTE-CONFIG's project directory. It auto-syncs to the remote."
  (let* ((remote-state (gethash (rsync-project--get-now-project-path)
                                rsync-project-states))
         (connectp (plist-get remote-state :connectp))
         (process (plist-get remote-state :process)))
    (if connectp
        (unless process
          (let ((rsync-buffer-name (format "*Rsync %s*" (cl-getf remote-config :root-path)))
                (rsync-args (rsync-project-build-rsync-args remote-config))
                (rsync-process nil)
                (default-directory (rsync-project--get-now-project-path)))
            (setq rsync-process
                  (apply
                   #'start-process
                   `("rsync-project"
                     ,rsync-buffer-name
                     "watchexec"
                     ,@(append (list "--ignore-nothing"
                                     "rsync")
                               rsync-args))))
            (with-current-buffer rsync-buffer-name
              (goto-char (point-max))
              (skip-chars-backward "\n[:space:]")
              (require 'time-stamp)
              (insert (concat "\n\n" (time-stamp-string) "\n")))
            (set-process-sentinel rsync-process
                                  #'(lambda (proc event)
                                      (if (or (s-contains? "kill" event) (s-contains? "finish" event))
                                          (message "%s rsync finish" (project-root (project-current)))
                                        (message "%s rsync run error: %s" (project-root (project-current)) event))))
            (puthash (rsync-project--get-now-project-path)
                     (plist-put remote-state
                                :process
                                rsync-process)
                     rsync-project-states)))
      (message "Can't connect remote, close auto save."))))

(defun rsync-project-auto-sync-stop (remote-config)
  "Stop the background monitor for REMOTE-CONFIG's project directory."
  (let* ((remote-state (gethash (rsync-project--get-now-project-path)
                                rsync-project-states))
         (process (plist-get remote-state :process)))
    (if process
        (progn
          (delete-process process)
          (puthash (rsync-project--get-now-project-path)
                   (plist-put remote-state
                              :process
                              nil)
                   rsync-project-states))
      (message "%s: Not start auto save process" (rsync-project--get-now-project-path)))))

(defun rsync-project-format-remote-config (remote-config)
  "Get REMOTE-CONFIG's remote path."
  (let* ((ssh-config (cl-getf remote-config :ssh-config))
         (user (cl-getf ssh-config :user))
         (host (cl-getf ssh-config :host))
         (port (cl-getf ssh-config :port))
         (path (cl-getf ssh-config :remote-dir)))
    (if (and user port)
        (format "%s@%s:%s:~/%s" user host port path)
      (format "%s:~/%s" host path))))

;;;###autoload
(defun rsync-project-re-auto-rsync ()
  "Rsync re connect auto rsync."
  (interactive)
  (rsync-project-read-list)
  (let ((remote-config (rsync-project-get-remote-config (rsync-project--get-now-project-path)))
        (remote-state (gethash (rsync-project--get-now-project-path)
                               rsync-project-states)))
    (if remote-config
        (when (cl-getf remote-config :auto-rsyncp)
          (let ((connectp (rsync-project--test-connection remote-config)))
            (puthash (rsync-project--get-now-project-path)
                     (plist-put remote-state
                                :connectp
                                connectp)
                     rsync-project-states)
            (rsync-project-auto-sync-stop remote-config)
            (rsync-project-auto-sync-start remote-config)
            (when connectp
              (message "Add rsync finish."))))
      (message "Need use add this project"))))

;;;###autoload
(defun rsync-project-auto-rsync-toggle ()
  "Toggle every project auto rsyncp."
  (interactive)
  (rsync-project-with-update-list remote-config
    (let ((auto-rsyncp (cl-getf remote-config :auto-rsyncp)))
      (if auto-rsyncp
          (rsync-project-auto-sync-stop remote-config)
        (rsync-project-auto-sync-start remote-config))
      (rsync-project--update-item remote-config
                                  (list :auto-rsyncp
                                        (not auto-rsyncp)))))
  (transient-setup 'rsync-project-dispatch))

;;;###autoload
(defun rsync-project-gitignorep-toggle ()
  "Toggle project gitignorep."
  (interactive)
  (rsync-project-with-update-list remote-config
    (rsync-project--update-item remote-config
                                (list :gitignorep
                                      (not (cl-getf remote-config :gitignorep)))))
  (call-interactively #'rsync-project-re-auto-rsync))

;;; menu
;;;###autoload (autoload 'rsync-project-dispatch "rsync-project-mode" nil t)
(transient-define-prefix rsync-project-dispatch ()
  "Rsync project menu."
  [:description
   rsync-project--selectd-project-description
   :pad-keys t
   ("c" "Connect remote" rsync-project-add :if-not rsync-project--check)
   ("d" "Delete remote" rsync-project-remove :if rsync-project--check)]
  ["Options"
   ("a" rsync-project--get-auto-rsyncp rsync-project-auto-rsync-toggle
    :transient t)
   ("g" rsync-project--get-gitignorep rsync-project-gitignorep-toggle
    :transient t)]
  [["Sync"
    :if rsync-project--check
    ("r" "Rsync all" rsync-project-sync-all)
    ("n" "Rsync re connect auto rsync" rsync-project-re-auto-rsync
     :if rsync-project--check-auto-rsync)]
   ["Ignore"
    :if rsync-project--check
    ("i a" "Add ignore" rsync-project-add-ignore
     :transient t)
    ("i r" "Remove ignore" rsync-project-remove-ignore
     :transient t)]]
  [("q" "Quit" transient-quit-one)]
  (interactive)
  (if (and (project-current) rsync-project-mode)
      (transient-setup 'rsync-project-dispatch)
    (message "not a project or open rsync-project-mode")))

(defun rsync-project--selectd-project-description ()
  "Return a Transient menu headline to indicate the currently selected project."
  (rsync-project-read-list)
  (let* ((root (project-root (project-current)))
         (remote-config (rsync-project-get-remote-config root))
         (remote-state (gethash (rsync-project--get-now-project-path)
                                rsync-project-states)))
    (format (propertize "Project: %s %s" 'face 'transient-heading)
            (if root
                (propertize root 'face 'transient-value)
              (propertize "None detected" 'face 'transient-inapt-suffix))
            (if remote-config
                (format (propertize "Remote: %s Connectp: %s Auto: %s \nIgnore list: %s" 'face 'transient-heading)
                        (propertize (rsync-project-format-remote-config remote-config) 'face 'transient-value)
                        (propertize (format "%s"
                                            (plist-get remote-state
                                                       :connectp))
                                    'face
                                    'transient-value)
                        (propertize (format "%s"
                                            (when (plist-get remote-state
                                                             :process)
                                              t))
                                    'face
                                    'transient-value)
                        (propertize (format "%s" (cl-getf remote-config :ignore-file-list)) 'face 'transient-help))
              ""))))

(defun rsync-project--get-auto-rsyncp ()
  "Return now project auto rsyncp state."
  (rsync-project-read-list)
  (let ((remote-config (rsync-project-get-remote-config (rsync-project--get-now-project-path))))
    (if remote-config
        (format "%s auto sync"
                (if (cl-getf remote-config :auto-rsyncp)
                    (propertize "Enable" 'face 'rsync-project-start-face)
                  (propertize "Disable" 'face 'rsync-project-stop-face)))
      (message "Need use add this project"))))

(defun rsync-project--get-gitignorep ()
  "Return now project gitignorep state."
  (rsync-project-read-list)
  (let ((remote-config (rsync-project-get-remote-config (rsync-project--get-now-project-path))))
    (if remote-config
        (format "%s filter gitignore"
                (if (cl-getf remote-config :gitignorep)
                    (propertize "Enable" 'face 'rsync-project-start-face)
                  (propertize "Disable" 'face 'rsync-project-stop-face)))
      (message "Need use add this project"))))

(provide 'rsync-project-mode)
;;; rsync-project-mode.el ends here
