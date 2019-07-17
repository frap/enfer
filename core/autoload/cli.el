;;; core/autoload/cli.el -*- lexical-binding: t; -*-

(require 'core-cli)

;;;###autoload
(defun enfer-cli-run (command &rest _args)
  (when (featurep 'general)
    (general-auto-unbind-keys))
  (let* ((evil-collection-mode-list nil)
         (default-directory enfer-emacs-dir)
         (buf (get-buffer-create " *bin/enfer*"))
         (enfer-message-backend 'ansi)
         (ignore-window-parameters t)
         (noninteractive t)
         (standard-output
          (lambda (char)
            (with-current-buffer buf
              (insert char)
              (when (memq char '(?\n ?\r))
                (ansi-color-apply-on-region (line-beginning-position -1) (line-end-position))
                (redisplay))))))
    (enfer-initialize t)
    (setq enfer-modules (enfer-modules))
    (enfer-initialize-modules t)
    (enfer-initialize-packages t)
    (with-current-buffer (switch-to-buffer buf)
      (erase-buffer)
      (require 'package)
      (redisplay)
      (enfer-dispatch command nil)
      (print! (green "\nDone!"))))
  (when (featurep 'general)
    (general-auto-unbind-keys 'undo))
  (message (format! (green "Done!"))))


;;;###autoload
(defun enfer//autoloads (&optional yes)
  "TODO"
  (interactive "P")
  (let ((enfer-auto-accept yes))
    (enfer-cli-run "autoloads")))

;;;###autoload
(defun enfer//update (&optional yes)
  "TODO"
  (interactive "P")
  (let ((enfer-auto-accept yes))
    (enfer-cli-run "update")))

;;;###autoload
(defun enfer//upgrade (&optional yes)
  "TODO"
  (interactive "P")
  (let ((enfer-auto-accept yes))
    (enfer-cli-run "upgrade"))
  (when (y-or-n-p "Vous devez redémarrer Emacs pour que la mise à niveau prenne effet. Restart?")
    (enfer/restart-and-restore)))

;;;###autoload
(defun enfer//install (&optional yes)
  "TODO"
  (interactive "P")
  (let ((enfer-auto-accept yes))
    (enfer-cli-run "install")))

;;;###autoload
(defun enfer//autoremove (&optional yes)
  "TODO"
  (interactive "P")
  (let ((enfer-auto-accept yes))
    (enfer-cli-run "autoremove")))

;;;###autoload
(defun enfer//refresh (&optional yes)
  "TODO"
  (interactive "P")
  (let ((enfer-auto-accept yes))
    (enfer-cli-run "refresh")))
