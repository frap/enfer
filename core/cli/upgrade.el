;;; core/cli/upgrade.el -*- lexical-binding: t; -*-

(dispatcher! (upgrade up) (enfer-upgrade)
  "Checks out the latest Enfer on this branch.

Doing so is equivalent to:

    cd ~/.emacs.d
    git pull
    bin/enfer clean
    bin/enfer refresh
    bin/enfer update")


;;
;; Quality of Life Commands

(defvar enfer-repo-url "https://github.com/hlissner/enfer-emacs"
  "TODO")
(defvar enfer-repo-remote "_upgrade"
  "TODO")

(defun enfer--working-tree-dirty-p (dir)
  (with-temp-buffer
    (let ((default-directory dir))
      (if (zerop (process-file "git" nil (current-buffer) nil
                               "status" "--porcelain" "-uno"))
          (string-match-p "[^ \t\n]" (buffer-string))
        (error "Failed to check working tree in %s" dir)))))

(defun enfer-upgrade ()
  "Upgrade Enfer to the latest version non-destructively."
  (require 'vc-git)
  (let* ((gitdir (expand-file-name ".git" enfer-emacs-dir))
         (branch (vc-git--symbolic-ref enfer-emacs-dir))
         (default-directory enfer-emacs-dir))
    (unless (file-exists-p gitdir)
      (error "Couldn't find %s. Was Enfer cloned properly?"
             (abbreviate-file-name gitdir)))
    (unless branch
      (error "Couldn't detect what branch you're using. Is Enfer detached?"))
    (when (enfer--working-tree-dirty-p enfer-emacs-dir)
      (user-error "Refusing to upgrade because %S has been modified. Stash or undo your changes"
                  (abbreviate-file-name enfer-emacs-dir)))
    (with-temp-buffer
      (let ((buf (current-buffer)))
        (condition-case-unless-debug e
            (progn
              (process-file "git" nil buf nil "remote" "remove" enfer-repo-remote)
              (unless (zerop (process-file "git" nil buf nil "remote" "add"
                                           enfer-repo-remote enfer-repo-url))
                (error "Failed to add %s to remotes" enfer-repo-remote))
              (unless (zerop (process-file "git" nil buf nil "fetch" "--tags"
                                           enfer-repo-remote branch))
                (error "Failed to fetch from upstream"))
              (let ((current-rev (vc-git-working-revision enfer-emacs-dir))
                    (rev (string-trim (shell-command-to-string (format "git rev-parse %s/%s" enfer-repo-remote branch)))))
                (unless rev
                  (error "Couldn't detect Enfer's version. Is %s a repo?"
                         (abbreviate-file-name enfer-emacs-dir)))
                (if (equal current-rev rev)
                    (message "Enfer is up to date!")
                  (message "Updates for Enfer are available!\n\n  Old revision: %s\n  New revision: %s\n"
                           current-rev rev)
                  (message "Comparision diff: https://github.com/hlissner/enfer-emacs/compare/%s...%s\n"
                           (substring current-rev 0 10) (substring rev 0 10))
                  ;; TODO Display newsletter diff
                  (unless (or enfer-auto-accept (y-or-n-p "Proceed?"))
                    (user-error "Aborted"))
                  (message "Removing byte-compiled files from your config (if any)")
                  (enfer-clean-byte-compiled-files)
                  (unless (zerop (process-file "git" nil buf nil "reset" "--hard"
                                               (format "%s/%s" enfer-repo-remote branch)))
                    (error "An error occurred while checking out the latest commit\n\n%s"
                           (buffer-string)))
                  (unless (equal (vc-git-working-revision enfer-emacs-dir) rev)
                    (error "Failed to checkout latest commit.\n\n%s" (buffer-string))))
                (enfer-refresh 'force-p)
                (when (enfer-packages-update enfer-auto-accept)
                  (enfer-reload-package-autoloads))
                (message "Done! Please restart Emacs for changes to take effect")))
          (user-error
           (message "%s Aborting." (error-message-string e)))
          (error
           (message "There was an unexpected error.\n\n%s\n\nOutput:\n%s"
                    e (buffer-string))))))))
