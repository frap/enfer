;;; core/autoload/scratch.el -*- lexical-binding: t; -*-

(defvar enfer-scratch-default-file "__default"
  "The default file name for a project-less scratch buffer.

Will be saved in `enfer-scratch-dir'.")

(defvar enfer-scratch-dir (concat enfer-etc-dir "scratch")
  "Where to save persistent scratch buffers.")

(defvar enfer-scratch-buffer-major-mode nil
  "What major mode to use in scratch buffers. This can be one of the
following:

  t           Inherits the major mode of the last buffer you had selected.
  nil         Uses `fundamental-mode'
  MAJOR-MODE  Any major mode symbol")

(defvar enfer-scratch-buffers nil
  "A list of active scratch buffers.")

(defvar-local enfer-scratch-current-project nil
  "The name of the project associated with the current scratch buffer.")

(defvar enfer-scratch-buffer-hook ()
  "The hooks to run after a scratch buffer is created.")

(defun enfer--load-persistent-scratch-buffer (name)
  (let ((scratch-file (expand-file-name (or name enfer-scratch-default-file)
                                        enfer-scratch-dir)))
    (make-directory enfer-scratch-dir t)
    (if (not (file-readable-p scratch-file))
        nil
      (erase-buffer)
      (insert-file-contents scratch-file)
      (set-auto-mode)
      t)))

;;;###autoload
(defun enfer-scratch-buffer (&optional mode directory project-name)
  "Return a scratchpad buffer in major MODE."
  (let* ((buffer-name (if project-name
                          (format "*enfer:scratch (%s)*" project-name)
                        "*enfer:scratch*"))
         (buffer (get-buffer buffer-name)))
    (with-current-buffer (get-buffer-create buffer-name)
      (unless buffer
        (setq buffer (current-buffer)
              default-directory directory
              enfer-scratch-current-project project-name)
        (setq enfer-scratch-buffers (cl-delete-if-not #'buffer-live-p enfer-scratch-buffers))
        (cl-pushnew buffer enfer-scratch-buffers)
        (enfer--load-persistent-scratch-buffer project-name)
        (when (and (eq major-mode 'fundamental-mode)
                   (functionp mode))
          (funcall mode))
        (add-hook 'kill-buffer-hook #'enfer|persist-scratch-buffer nil 'local)
        (run-hooks 'enfer-scratch-buffer-created-hook))
      buffer)))


;;
;;; Persistent scratch buffer

;;;###autoload
(defun enfer|persist-scratch-buffer ()
  "Save the current buffer to `enfer-scratch-dir'."
  (write-region
   (point-min) (point-max)
   (expand-file-name (or enfer-scratch-current-project enfer-scratch-default-file)
                     enfer-scratch-dir)))

;;;###autoload
(defun enfer|persist-scratch-buffers ()
  "Save all scratch buffers to `enfer-scratch-dir'."
  (setq enfer-scratch-buffers (cl-delete-if-not #'buffer-live-p enfer-scratch-buffers))
  (dolist (buffer enfer-scratch-buffers)
    (with-current-buffer buffer
      (enfer|persist-scratch-buffer))))

;;;###autoload
(unless noninteractive
  (add-hook 'kill-emacs-hook #'enfer|persist-scratch-buffers))


;;
;;; Commands

;;;###autoload
(defun enfer/open-scratch-buffer (&optional arg project-p)
  "Opens the (persistent) scratch buffer in a popup.

If passed the prefix ARG, switch to it in the current window.
If PROJECT-P is non-nil, open a persistent scratch buffer associated with the
  current project."
  (interactive "P")
  (let (projectile-enable-caching)
    (funcall
     (if arg
         #'switch-to-buffer
       #'pop-to-buffer)
     (enfer-scratch-buffer
      (cond ((eq enfer-scratch-buffer-major-mode t)
             (unless (or buffer-read-only
                         (derived-mode-p 'special-mode)
                         (string-match-p "^ ?\\*" (buffer-name)))
               major-mode))
            ((null enfer-scratch-buffer-major-mode)
             nil)
            ((symbolp enfer-scratch-buffer-major-mode)
             enfer-scratch-buffer-major-mode))
      default-directory
      (when project-p
        (enfer-project-name))))))

;;;###autoload
(defun enfer/switch-to-scratch-buffer (&optional project-p)
  "Like `enfer/open-scratch-buffer', but switches to it in the current window."
  (interactive)
  (enfer/open-scratch-buffer t))

;;;###autoload
(defun enfer/open-project-scratch-buffer (&optional arg)
  "Opens the (persistent) project scratch buffer in a popup.

If passed the prefix ARG, switch to it in the current window."
  (interactive "P")
  (enfer/open-scratch-buffer arg 'project))

;;;###autoload
(defun enfer/switch-to-project-scratch-buffer ()
  "Like `enfer/open-project-scratch-buffer', but switches to it in the current
window."
  (interactive)
  (enfer/open-project-scratch-buffer t))

;;;###autoload
(defun enfer/revert-scratch-buffer ()
  "Revert scratch buffer to last persistent state."
  (interactive)
  (unless (string-match-p "^\\*enfer:scratch" (buffer-name))
    (user-error "Not in a scratch buffer"))
  (when (enfer--load-persistent-scratch-buffer enfer-scratch-current-project)
    (message "Reloaded scratch buffer")))

;;;###autoload
(defun enfer/delete-persistent-scratch-file (&optional arg)
  "Deletes a scratch buffer file in `enfer-scratch-dir'.

If prefix ARG, delete all persistent scratches."
  (interactive)
  (if arg
      (progn
        (delete-directory enfer-scratch-dir t)
        (message "Cleared %S" (abbreviate-file-name enfer-scratch-dir)))
    (make-directory enfer-scratch-dir t)
    (let ((file (read-file-name "Delete scratch file > " enfer-scratch-dir "scratch")))
      (if (not (file-exists-p file))
          (message "%S does not exist" (abbreviate-file-name file))
        (delete-file file)
        (message "Successfully deleted %S" (abbreviate-file-name file))))))
