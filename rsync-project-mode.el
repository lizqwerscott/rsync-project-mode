;;; rsync-project-mode.el --- Rsync project to remote machines  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.4") (project "0.9.8") (transient "0.4.3"))
;; Keywords: tools
;; URL: https://github.com/lizqwerscott/rsync-project-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; rsync project mode

;;; Code:

(require 'cl-lib)
(require 'tramp)

(require 'project)
(require 'transient)

(defclass rsync-project-state ()
  ((connectp
    :initarg :connectp
    :type boolean
    :accessor rsync-project-state-connectp)
   (process
    :initform nil
    :accessor rsync-project-state-process)
   (sync-state
    :initarg :sync-state
    :accessor rsync-project-state-sync-state)
   (debounce-timer
    :initform nil
    :accessor rsync-project-state-debounce-timer))
  "State information for a rsync project.
- Disconnected: not connected with remote
- Sync: local code sync with remote code
- Wait: Waiting for debounce period before syncing
- Syncing: Currently syncing files to remote
- Failed: Last sync operation failed")

(defgroup rsync-project nil
  "Convenient project remote synchronization."
  :group 'convenience
  :prefix "rsync-project-"
  :link '(url-link "https://github.com/lizqwerscott/rsync-project-mode"))

(defcustom rsync-project-default-auto-rsyncp nil
  "Non-nil means auto sync in background when creating new remote project."
  :group 'rsync-project
  :type 'boolean)

(defcustom rsync-project-default-gitignorep nil
  "Non-nil means use `.gitignore' as filter when creating new remote project."
  :group 'rsync-project
  :type 'boolean)

(defcustom rsync-project-list-file (if (boundp 'no-littering-var-directory )
                                       (file-name-concat no-littering-var-directory
                                                         "rsync-project-list-file.el")
                                     (file-name-concat user-emacs-directory
                                                       "rsync-project-list-file.el"))
  "File in which to save the list of known projects."
  :group 'rsync-project
  :type 'file)

(defcustom rsync-project-cooldown-period 2.0
  "Time in seconds to wait after last file change before auto-syncing.
This cooldown period prevents too frequent rsync operations when
multiple files are changed in quick succession."
  :group 'rsync-project
  :type 'number)

(defvar rsync-project-remote-list nil
  "List of project rsync remote server.")

(defvar-local rsync-project-mode nil
  "Whether rsync-mode is enabled.")

(defvar rsync-project-states (make-hash-table :test #'equal)
  "Rsync project remote states and process.")

(defface rsync-project-start-face
  '((t :foreground "green"))
  "Face for `Start' state.")

(defface rsync-project-stop-face
  '((t :foreground "red"))
  "Face for `Stop' state.")

(defface rsync-project-disconnected-face
  '((t :foreground "red" :weight bold))
  "Face for disconnected state.")

(defface rsync-project-sync-face
  '((t :foreground "green" :weight bold))
  "Face for idle state.")

(defface rsync-project-wait-face
  '((t :foreground "yellow" :weight bold))
  "Face for waiting state.")

(defface rsync-project-syncing-face
  '((t :foreground "cyan" :weight bold))
  "Face for syncing state.")

(defface rsync-project-failed-face
  '((t :foreground "red" :weight bold))
  "Face for failed state.")

(define-minor-mode rsync-project-mode
  "Toggle rsync project mode."
  :init-value nil
  :group 'rsync-project
  (rsync-project-read-list)
  (when-let* ((project-root (project-current))
              (path (rsync-project--get-now-project-path))
              (remote-config (rsync-project-get-remote-config path)))
    (let ((remote-state (gethash path rsync-project-states)))
      (unless remote-state
        (let ((connectp (rsync-project--test-connection remote-config)))
          (unless connectp
            (message "%s remote can't connect"
                     (plist-get (plist-get remote-config :ssh-config)
                                :host)))
          (setq remote-state
                (make-instance 'rsync-project-state
                               :connectp connectp
                               :sync-state (if connectp
                                               'sync
                                             'disconnected)))
          (puthash path remote-state rsync-project-states)))
      (if (not rsync-project-mode)
          (rsync-project-auto-sync-stop remote-config remote-state)
        (when (plist-get remote-config :auto-rsyncp)
          (rsync-project-auto-sync-start remote-config remote-state))))))

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
  "Update PLIST with values from NEW-KEY-VALUES for matching keys.
Return a new plist containing all keys from PLIST, with values
replaced by corresponding values in NEW-KEY-VALUES when present."
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
  "Return the absolute path of the current project's root directory."
  (when-let* ((project-now (project-current))
              (root (project-root project-now)))
    (file-truename root)))

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
Returns t if connection succeeds, nil otherwise."
  (let* ((ssh-config (cl-getf remote-config :ssh-config))
         (remote-user (cl-getf ssh-config :user))
         (remote-host (cl-getf ssh-config :host))
         (remote-port (cl-getf ssh-config :port)))
    (= 0
       (shell-command
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
         (remote-path (cl-getf ssh-config :remote-dir)))
    `("-avtP"
      ,@(if remote-port
            (if (not (= 22 remote-port))
                `("-e"
                  (format "\"ssh -p %d\"" remote-port))))
      ,@(if (cl-getf remote-config :gitignorep)
            (list "--filter=':- .gitignore'"))
      ,@(mapcar #'(lambda (dir)
                    (concat "--exclude=" dir))
                (cl-getf remote-config :ignore-file-list))
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
                            (string-join (make-list (length rsync-args)
                                                    "%s")
                                         " ")))
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
                    (file-name-concat remote-dir-path name)))
            (while add-ignore-filep
              (cl-pushnew (file-relative-name (read-file-name "Ignore path:" project-root-dir)
                                              project-root-dir)
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
  (let* ((path (rsync-project--get-now-project-path))
         (remote-config (rsync-project-get-remote-config path))
         (remote-state (gethash path rsync-project-states)))
    (if remote-config
        (progn
          (when (cl-getf remote-config :auto-rsyncp)
            (rsync-project-auto-sync-stop remote-config remote-state))
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
  "Add ignore patterns to current project's rsync configuration.
This function interactively adds file/directory patterns to the
ignore list of the current project's remote configuration."
  (interactive)
  (rsync-project-with-update-list remote-config
    (let ((add-ignore-filep t)
          (new-ignore-file-list (cl-getf remote-config :ignore-file-list))
          (project-root-dir (cl-getf remote-config :root-path)))
      (while add-ignore-filep
        (cl-pushnew (file-relative-name (read-file-name "Ignore path:" project-root-dir)
                                        project-root-dir)
                    new-ignore-file-list)
        (setf add-ignore-filep
              (yes-or-no-p (format "(%s)Add ignore files?" new-ignore-file-list))))
      (rsync-project--update-item remote-config
                                  (list :ignore-file-list new-ignore-file-list))))
  (call-interactively #'rsync-project-re-auto-rsync))

;;;###autoload
(defun rsync-project-remove-ignore ()
  "Remove ignore patterns from current project's rsync configuration.
This function interactively removes file/directory patterns from
the ignore list of the current project's remote configuration."
  (interactive)
  (rsync-project-with-update-list remote-config
    (let* ((new-ignore-file-list (cl-getf remote-config :ignore-file-list))
           (choice (completing-read "Choose an ignore: " new-ignore-file-list)))
      (rsync-project--update-item remote-config
                                  (list :ignore-file-list
                                        (cl-remove-if #'(lambda (remote-config)
                                                          (string= choice
                                                                   remote-config))
                                                      new-ignore-file-list)))))
  (call-interactively #'rsync-project-re-auto-rsync))

;;;###autoload
(defun rsync-project-change-remote ()
  "Change remote directory for current project."
  (interactive)
  (rsync-project-read-list)
  (let* ((path (rsync-project--get-now-project-path))
         (name (project-name (project-current)))
         (remote-config (rsync-project-get-remote-config path)))
    (if remote-config
        (let* ((ssh-config (cl-getf remote-config :ssh-config))
               (old-remote-dir (cl-getf ssh-config :remote-dir))
               (new-remote-dir (tramp-dissect-file-name (read-file-name "New remote dir:" "/ssh:")))
               (new-remote-dir-path (tramp-file-name-localname new-remote-dir)))
          (unless (string= (file-name-nondirectory (directory-file-name new-remote-dir-path))
                           name)
            (setf new-remote-dir-path
                  (file-name-concat new-remote-dir-path name)))
          (rsync-project-with-update-list remote-config
            (rsync-project--update-item remote-config
                                        (list :ssh-config
                                              (list :user (tramp-file-name-user new-remote-dir)
                                                    :host (tramp-file-name-host new-remote-dir)
                                                    :port (tramp-file-name-port new-remote-dir)
                                                    :remote-dir new-remote-dir-path))))
          (message "Change %s to %s" old-remote-dir new-remote-dir-path))
      (message "Now project not add rsync"))))

;;;###autoload
(defun rsync-project-sync-all ()
  "Rsync all."
  (interactive)
  (when-let* ((path (rsync-project--get-now-project-path))
              (remote-config (rsync-project-get-remote-config path))
              (remote-state (gethash path rsync-project-states)))
    (rsync-project--reset-debounce-timer remote-config remote-state)))

;;; auto sync
(defun rsync-project-auto-sync-start (remote-config remote-state)
  "Start background monitoring for automatic syncing.
REMOTE-CONFIG is the project's remote configuration plist.
REMOTE-STATE is the project's state object."
  (if (rsync-project-state-connectp remote-state)
      (unless (rsync-project-state-process remote-state)
        (let* ((path (cl-getf remote-config :root-path))
               (rsync-buffer-name (format " *Rsync %s*" path))
               (rsync-process nil)
               (default-directory path))
          (setq rsync-process
                (apply
                 #'start-process
                 `("rsync-project"
                   ,rsync-buffer-name
                   "watchexec"
                   "--emit-events-to=json-stdio"
                   "--only-emit-events")))
          (set-process-sentinel rsync-process
                                #'(lambda (_ event)
                                    (if (or (string-match-p "kill" event) (string-match-p "finish" event))
                                        (message "%s rsync finish" (project-root (project-current)))
                                      (message "%s rsync run error: %s" (project-root (project-current)) event))))
          (set-process-filter rsync-process #'rsync-project-auto-sync-filter)
          (process-put rsync-process 'rsync-project-path path)
          (setf (rsync-project-state-sync-state remote-state)
                'sync)
          (setf (rsync-project-state-process remote-state)
                rsync-process)))
    (message "Can't connect remote, close auto save.")))

(defun rsync-project-auto-sync-stop (remote-config remote-state)
  "Stop background monitoring for automatic syncing.
REMOTE-CONFIG is the project's remote configuration plist.
REMOTE-STATE is the project's state object."
  (let* ((process (rsync-project-state-process remote-state)))
    (if process
        (progn
          (delete-process process)
          (setf (rsync-project-state-process remote-state) nil)
          (setf (rsync-project-state-sync-state remote-state) 'disconnected))
      (message "%s: Not start auto save process" (cl-getf remote-config :root-path)))))

(defun rsync-project--auto-sync (remote-config remote-state)
  "Perform an rsync operation for the current project.
REMOTE-CONFIG is the project's remote configuration plist.
REMOTE-STATE is the project's state object. This function is
called after the debounce period expires and actually runs the
rsync command to sync the project to the remote server."
  (let* ((path (cl-getf remote-config :root-path))
         (rsync-buffer-name (format "* Rsync sync %s*" path))
         (rsync-process nil)
         (default-directory path))
    (setf (rsync-project-state-sync-state remote-state) 'running)
    (setq rsync-process
          (apply
           #'start-process
           `("rsync-project-sync"
             ,rsync-buffer-name
             "bash"
             "-c"
             ,(rsync-project-generate-rsync-cmd remote-config))))
    (set-process-sentinel rsync-process
                          (lambda (_ event)
                            (if (or (string-match-p "kill" event) (string-match-p "finish" event))
                                (setf (rsync-project-state-sync-state remote-state) 'sync)
                              (message "%s rsync run error: %s" (project-root (project-current)) event)
                              (setf (rsync-project-state-sync-state remote-state) 'failed))))))

(defun rsync-project--reset-debounce-timer (remote-config remote-state)
  "Reset the debounce timer for automatic syncing.
REMOTE-CONFIG is the project's remote configuration plist.
REMOTE-STATE is the project's state object. This function
cancels any existing debounce timer and starts a new one with
`rsync-project-cooldown-period` duration."
  (let* ((debounce-timer (rsync-project-state-debounce-timer remote-state)))
    (when debounce-timer
      (cancel-timer debounce-timer))
    (setf (rsync-project-state-debounce-timer remote-state)
          (run-at-time rsync-project-cooldown-period
                       nil
                       (lambda ()
                         (setf (rsync-project-state-debounce-timer remote-state)
                               nil)
                         (rsync-project--auto-sync remote-config remote-state))))))

(defun rsync-project-auto-sync-filter (proc string)
  "Process filter for auto-sync watchexec process.
PROC is the watchexec process. STRING is the JSON output from
watchexec about file changes. When file changes are detected, it
resets the debounce timer which will eventually trigger rsync."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (dolist (data-block (split-string string "\n"))
        (let* ((data-block (string-trim data-block))
               (json nil))
          (unless (string= data-block "")
            (condition-case-unless-debug err
                (setq json
                      (json-parse-string data-block
                                         :object-type 'plist
                                         :null-object nil
                                         :false-object :json-false))
              (json-parse-error
               ;; parse error and not because of incomplete json
               (user-error "Invalid JSON: %s\t %s" (cdr err) data-block)))
            (when json
              (let* ((path (process-get proc 'rsync-project-path))
                     (remote-config (rsync-project-get-remote-config path))
                     (remote-state (gethash path rsync-project-states)))
                (setf (rsync-project-state-sync-state remote-state) 'check)
                (rsync-project--reset-debounce-timer remote-config remote-state)))))))))

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
  "Re-establish auto-sync connection for current project.
This function tests the remote connection, stops any running
auto-sync process, and restarts it if the connection is successful."
  (interactive)
  (rsync-project-read-list)
  (when-let* ((path (rsync-project--get-now-project-path))
              (remote-config (rsync-project-get-remote-config path))
              (remote-state (gethash path rsync-project-states)))
    (if remote-config
        (when (cl-getf remote-config :auto-rsyncp)
          (let ((connectp (rsync-project--test-connection remote-config)))
            (setf (rsync-project-state-connectp remote-state) connectp)
            (rsync-project-auto-sync-stop remote-config remote-state)
            (rsync-project-auto-sync-start remote-config remote-state)
            (when connectp
              (message "Add rsync finish."))))
      (message "Need use add this project"))))

;;;###autoload
(defun rsync-project-auto-rsync-toggle ()
  "Toggle auto-sync for now projects.
This function toggles the :auto-rsyncp setting for all projects
in `rsync-project-remote-list` and starts/stops the auto-sync
process accordingly."
  (interactive)
  (rsync-project-with-update-list remote-config
    (let ((auto-rsyncp (cl-getf remote-config :auto-rsyncp))
          (remote-state (gethash (cl-getf remote-config :root-path)
                                 rsync-project-states)))
      (if auto-rsyncp
          (rsync-project-auto-sync-stop remote-config remote-state)
        (rsync-project-auto-sync-start remote-config remote-state))
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
   ("d" "Delete remote" rsync-project-remove :if rsync-project--check)
   ("o" "Change remote" rsync-project-change-remote :if rsync-project--check)]
  ["Options"
   ("a" rsync-project-auto-rsync-toggle
    :description rsync-project--get-auto-rsyncp
    :transient t)
   ("g" rsync-project-gitignorep-toggle
    :description rsync-project--get-gitignorep
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
  (when-let* ((path (rsync-project--get-now-project-path))
              (remote-config (rsync-project-get-remote-config path))
              (remote-state (gethash path rsync-project-states)))
    (format (propertize "Project: %s %s" 'face 'transient-heading)
            (if path
                (propertize path 'face 'transient-value)
              (propertize "None detected" 'face 'transient-inapt-suffix))
            (if remote-config
                (format (propertize "Remote: %s Connectp: %s Auto: %s \nIgnore list: %s" 'face 'transient-heading)
                        (propertize (rsync-project-format-remote-config remote-config) 'face 'transient-value)
                        (propertize (format "%s"
                                            (rsync-project-state-connectp remote-state))
                                    'face
                                    'transient-value)
                        (propertize (format "%s"
                                            (when (rsync-project-state-process remote-state)
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

;;; modeline
(defun rsync-project--indicator ()
  "Return a string indicating current rsync state for mode line."
  (when-let* ((path (rsync-project--get-now-project-path))
              (remote-state (gethash path rsync-project-states))
              (sync-states (rsync-project-state-sync-state remote-state)))
    (format " %s "
            (pcase sync-states
              ('disconnected (propertize "❌ Disconnected" 'face 'rsync-project-disconnected-face))
              ('sync (propertize "✅ Sync" 'face 'rsync-project-sync-face))
              ('check (propertize "⏳ Waiting" 'face 'rsync-project-wait-face))
              ('running (propertize "⚡ Syncing" 'face 'rsync-project-syncing-face))
              ('failed (propertize "❌ Failed" 'face 'rsync-project-failed-face))))))

;;;###autoload
(defun rsync-project-setup-indicator ()
  "Add rsync status indicator to the mode line.
This function adds a visual indicator showing the current rsync state:
The indicator will appear in the mode line when `rsync-project-mode' is active."
  (unless (cl-find '(rsync-project-mode (:eval (rsync-project--indicator))) mode-line-misc-info :test 'equal)
    (add-to-list 'mode-line-misc-info
                 '(rsync-project-mode (:eval (rsync-project--indicator))))))

(provide 'rsync-project-mode)
;;; rsync-project-mode.el ends here
