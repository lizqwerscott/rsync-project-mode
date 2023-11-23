
;;; rsync-project-mode.el --- Rsync project to remote machines  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2023  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; URL: https://github.com/lizqwerscott/rsync-project-mode
;; Version: 0.1.0
;; Keywords: comm

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'f)
(require 's)

(defgroup rsync-project nil
  "Convenient project remote synchronization."
  :group 'convenience
  :prefix "rsync-project-"
  :link '(url-link "https://github.com/lizqwerscott/rsync-project-mode"))

(defcustom rsync-project-sync-on-save nil
  "Whether to activate a hook that synchronizes the project after each save."
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

(defvar-local rsync-project--process nil
  "Rsync process object.")

(defvar rsync-project--process-exit-hook nil
  "Closure defining the process cleanup code.")

(define-minor-mode rsync-project-mode
  "Toggle rsync project mode"
  :init-value nil
  :group 'rsync-project
  (rsync-project-read-list)
  (let ((project-now (project-current)))
    (when project-now
      (if (rsync-project-get-remote-config (project-root project-now))
          (if (not rsync-project-mode)
              (remove-hook 'after-save-hook #'rsync-project-auto-sync t)
            (when rsync-project-sync-on-save
              (add-hook 'after-save-hook #'rsync-project-auto-sync 0 t)))
        ;; (message "Failed to activate rsync-project-mode: No remote configuration for rsync-project-mode found.")
        ))))

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
             (lambda (elt) (stringp (car-safe elt)))
             rsync-project-remote-list)
      (warn "Contents of %s are in wrong format, resetting"
            rsync-project-list-file)
      (setq rsync-project-remote-list nil))))

(defun rsync-project-get-remote-config (project-path)
  (find (file-truename project-path)
        rsync-project-remote-list
        :key #'(lambda (elm)
                 (file-truename
                  (first elm)))
        :test #'string=))

(defun rsync-project--test-connection (remote-config)
  (let ((remote-user (first (second remote-config)))
        (remote-host (second (second remote-config)))
        (remote-port (third (second remote-config))))
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
  (let ((local-path (first remote-config))
        (remote-user (first (second remote-config)))
        (remote-host (second (second remote-config)))
        (remote-port (third (second remote-config)))
        (remote-path (fourth (second remote-config)))
        (ignore-list (third remote-config)))
    `("-avtP"
      ,@(if remote-port
            (if (not (= 22 remote-port))
                `("-e"
                  (format "\"ssh -p %d\"" remote-port))))
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

(defun rsync-project-generate-rsync-cmd (ssh-config)
  (let ((local-path (first ssh-config))
        (remote-user (first (second ssh-config)))
        (remote-host (second (second ssh-config)))
        (remote-port (third (second ssh-config)))
        (remote-path (fourth (second ssh-config)))
        (ignore-list (third ssh-config)))
    (format "rsync -avtP %s %s %s %s"
            (if remote-port
                (if (not (= 22 remote-port))
                    (format "-e \"ssh -p %d \"" remote-port)
                  )
              "")
            (string-join
             (mapcar #'(lambda (dir)
                         (concat "--exclude=" dir))
                     ignore-list)
             " ")
            local-path
            (if (and remote-user remote-host)
                (format "%s@%s:%s"
                        remote-user
                        remote-host
                        remote-path)
              (format "%s:%s"
                      remote-host
                      remote-path)))))

;;;###autoload
(defun rsync-project-add ()
  "Add now project to rsync list"
  (interactive)
  (let ((project-root-dir (file-truename (project-root (project-current))))
        (name (project-name (project-current))))
    (if (not (rsync-project-get-remote-config project-root-dir))
        (let ((ignore-file-list (list ".git"))
              (remote-dir (tramp-dissect-file-name (read-file-name "Remote dir:" "/ssh:")))
              (add-ignore-filep (yes-or-no-p "Add ignore files:")))
          (let ((remote-dir-path (tramp-file-name-localname remote-dir)))
            (when (not (string= (file-name-base remote-dir-path)
                              name))
              (setf remote-dir-path
                    (f-join remote-dir-path name)))
            (while add-ignore-filep
              (add-to-list 'ignore-file-list
                           (f-filename (read-file-name "Ignore path:" project-root-dir)))
              (setf add-ignore-filep
                    (yes-or-no-p (format "(%s)Add ignore files:" ignore-file-list))))
            (add-to-list 'rsync-project-remote-list
                         (list project-root-dir
                               (list (tramp-file-name-user remote-dir)
                                     (tramp-file-name-host remote-dir)
                                     (tramp-file-name-port remote-dir)
                                     remote-dir-path)
                               ignore-file-list))
            (rsync-project-write-list)))
      (message "Already add now project."))))

;;;###autoload
(defun rsync-project-remove ()
  "Remove now project in rsync list"
  (interactive)
  (rsync-project-read-list)
  (let ((ssh-config (rsync-project-get-remote-config (project-root (project-current)))))
    (if ssh-config
        (progn
          (setf rsync-project-remote-list
                (cl-remove-if #'(lambda (item)
                                  (string= (first item)
                                           (first ssh-config)))
                              rsync-project-remote-list))
          (rsync-project-write-list))
      (message "Now project not add rsync"))))

;;;###autoload
(defun rsync-project-sync-all ()
  "Rsync all."
  (interactive)
  (let ((ssh-config (rsync-project-get-remote-config (project-root (project-current)))))
    (if ssh-config
        (let ((rsync-buffer (get-buffer-create "*Rsync project*")))
          (async-shell-command (rsync-project-generate-rsync-cmd ssh-config)
                               rsync-buffer))
      (message "Need use add this project"))))

(defun rsync-project--run (remote-config)
  (if rsync-project--process
      (message "Cannot start a new rsync process until the existing one finishes.")
    (let ((rsync-buffer-name (format "*Rsync %s*" (f-filename (first remote-config)))))
      (setq rsync-project--process
            (apply
             #'start-process
             `("rsync-project"
               ,rsync-buffer-name
               "rsync"
               ,@(rsync-project-build-rsync-args remote-config))))
      (with-current-buffer rsync-buffer-name
        (goto-char (point-max))
        (skip-chars-backward "\n[:space:]")
        (require 'time-stamp)
        (insert (concat "\n\n" (time-stamp-string) "\n")))
      (setq rsync-project--process-exit-hook
            (lambda (_ event)
              (setq rsync-project--process nil)
              (with-current-buffer (current-buffer)
                (when rsync-project-mode
                  ;; (spinner-stop rsync--spinner)
                  )
                )
              (if (string-equal event "finished\n")
                  (message "Rsync complete.")
                (message "Rsync process received abnormal event %s" event)
                (message "Close auto save.")
                (remove-hook 'after-save-hook #'rsync-project-auto-sync t))))
      (set-process-sentinel rsync-project--process
                            #'(lambda (proc event)
                                (funcall rsync-project--process-exit-hook proc event))))))

(defun rsync-project-auto-sync ()
  (let ((remote-config (rsync-project-get-remote-config (project-root (project-current)))))
    (if remote-config
        (if (rsync-project--test-connection remote-config)
            (rsync-project--run remote-config)
          (message "Can't connect remote, close auto save.")
          (remove-hook 'after-save-hook #'rsync-project-auto-sync t))
      (message "Need use add this project"))))

;;;###autoload
(defun rsync-project-show-remote-config ()
  "Show now project remote config."
  (interactive)
  (rsync-project-read-list)
  (message "remote: %s" (second (rsync-project-get-remote-config (project-root (project-current))))))

;;;###autoload
(defun rsync-project-re-auto-rsync ()
  "Rsync re connect auto rsync."
  (interactive)
  (setq rsync-project--process nil)
  (when rsync-project-sync-on-save
    (add-hook 'after-save-hook #'rsync-project-auto-sync 0 t)
    (message "Add rsync finish.")))

(provide 'rsync-project-mode)
;;; rsync-project-mode.el ends here
