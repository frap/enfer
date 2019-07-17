;;; core/autoload/buffers.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar enfer-real-buffer-functions
  '(enfer-dired-buffer-p)
  "A list of predicate functions run to determine if a buffer is real, unlike
`enfer-unreal-buffer-functions'. They are passed one argument: the buffer to be
tested.

Should any of its function returns non-nil, the rest of the functions are
ignored and the buffer is considered real.

See `enfer-real-buffer-p' for more information.")

;;;###autoload
(defvar enfer-unreal-buffer-functions
  '(minibufferp enfer-special-buffer-p enfer-non-file-visiting-buffer-p)
  "A list of predicate functions run to determine if a buffer is *not* real,
unlike `enfer-real-buffer-functions'. They are passed one argument: the buffer to
be tested.

Should any of these functions return non-nil, the rest of the functions are
ignored and the buffer is considered unreal.

See `enfer-real-buffer-p' for more information.")

;;;###autoload
(defvar-local enfer-real-buffer-p nil
  "If non-nil, this buffer should be considered real no matter what. See
`enfer-real-buffer-p' for more information.")

;;;###autoload
(defvar enfer-fallback-buffer-name "*scratch*"
  "The name of the buffer to fall back to if no other buffers exist (will create
it if it doesn't exist).")


;;
;; Functions

;;;###autoload
(defun enfer-buffer-frame-predicate (buf)
  "To be used as the default frame buffer-predicate parameter. Returns nil if
BUF should be skipped over by functions like `next-buffer' and `other-buffer'."
  (or (enfer-real-buffer-p buf)
      (eq buf (enfer-fallback-buffer))))

;;;###autoload
(defun enfer-fallback-buffer ()
  "Returns the fallback buffer, creating it if necessary. By default this is the
scratch buffer. See `enfer-fallback-buffer-name' to change this."
  (let (buffer-list-update-hook)
    (get-buffer-create enfer-fallback-buffer-name)))

;;;###autoload
(defalias 'enfer-buffer-list #'buffer-list)

;;;###autoload
(defun enfer-project-buffer-list (&optional project)
  "Return a list of buffers belonging to the specified PROJECT.

If PROJECT is nil, default to the current project.

If no project is active, return all buffers."
  (let ((buffers (enfer-buffer-list)))
    (if-let* ((project-root
               (if project (expand-file-name project)
                 (enfer-project-root))))
        (cl-loop for buf in buffers
                 if (projectile-project-buffer-p buf project-root)
                 collect buf)
      buffers)))

;;;###autoload
(defun enfer-open-projects ()
  "Return a list of projects with open buffers."
  (cl-loop with projects = (make-hash-table :test 'equal :size 8)
           for buffer in (enfer-buffer-list)
           if (buffer-live-p buffer)
           if (enfer-real-buffer-p buffer)
           if (with-current-buffer buffer (enfer-project-root))
           do (puthash (abbreviate-file-name it) t projects)
           finally return (hash-table-keys projects)))

;;;###autoload
(defun enfer-dired-buffer-p (buf)
  "Returns non-nil if BUF is a dired buffer."
  (with-current-buffer buf (derived-mode-p 'dired-mode)))

;;;###autoload
(defun enfer-special-buffer-p (buf)
  "Returns non-nil if BUF's name starts and ends with an *."
  (equal (substring (buffer-name buf) 0 1) "*"))

;;;###autoload
(defun enfer-temp-buffer-p (buf)
  "Returns non-nil if BUF is temporary."
  (equal (substring (buffer-name buf) 0 1) " "))

;;;###autoload
(defun enfer-non-file-visiting-buffer-p (buf)
  "Returns non-nil if BUF does not have a value for `buffer-file-name'."
  (not (buffer-file-name buf)))

;;;###autoload
(defun enfer-real-buffer-list (&optional buffer-list)
  "Return a list of buffers that satify `enfer-real-buffer-p'."
  (cl-remove-if-not #'enfer-real-buffer-p (or buffer-list (enfer-buffer-list))))

;;;###autoload
(defun enfer-real-buffer-p (buffer-or-name)
  "Returns t if BUFFER-OR-NAME is a 'real' buffer.

A real buffer is a useful buffer; a first class citizen in Enfer. Real ones
should get special treatment, because we will be spending most of our time in
them. Unreal ones should be low-profile and easy to cast aside, so we can focus
on real ones.

The exact criteria for a real buffer is:

  1. A non-nil value for the buffer-local value of the `enfer-real-buffer-p'
     variable OR
  2. Any function in `enfer-real-buffer-functions' returns non-nil OR
  3. None of the functions in `enfer-unreal-buffer-functions' must return
     non-nil.

If BUFFER-OR-NAME is omitted or nil, the current buffer is tested."
  (or (bufferp buffer-or-name)
      (stringp buffer-or-name)
      (signal 'wrong-type-argument (list '(bufferp stringp) buffer-or-name)))
  (when-let (buf (get-buffer buffer-or-name))
    (and (buffer-live-p buf)
         (not (enfer-temp-buffer-p buf))
         (or (buffer-local-value 'enfer-real-buffer-p buf)
             (run-hook-with-args-until-success 'enfer-real-buffer-functions buf)
             (not (run-hook-with-args-until-success 'enfer-unreal-buffer-functions buf))))))

;;;###autoload
(defun enfer-unreal-buffer-p (buffer-or-name)
  "Return t if BUFFER-OR-NAME is an 'unreal' buffer.

See `enfer-real-buffer-p' for details on what that means."
  (not (enfer-real-buffer-p buffer-or-name)))

;;;###autoload
(defun enfer-buffers-in-mode (modes &optional buffer-list derived-p)
  "Return a list of buffers whose `major-mode' is `eq' to MODE(S).

If DERIVED-P, test with `derived-mode-p', otherwise use `eq'."
  (let ((modes (enfer-enlist modes)))
    (cl-remove-if-not (if derived-p
                          (lambda (buf)
                            (with-current-buffer buf
                              (apply #'derived-mode-p modes)))
                        (lambda (buf)
                          (memq (buffer-local-value 'major-mode buf) modes)))
                      (or buffer-list (enfer-buffer-list)))))

;;;###autoload
(defun enfer-visible-windows (&optional window-list)
  "Return a list of the visible, non-popup (dedicated) windows."
  (cl-loop for window in (or window-list (window-list))
           when (or (window-parameter window 'visible)
                    (not (window-dedicated-p window)))
           collect window))

;;;###autoload
(defun enfer-visible-buffers (&optional buffer-list)
  "Return a list of visible buffers (i.e. not buried)."
  (cl-loop for buf in (or buffer-list (enfer-buffer-list))
           when (get-buffer-window buf)
           collect buf))

;;;###autoload
(defun enfer-buried-buffers (&optional buffer-list)
  "Get a list of buffers that are buried."
  (cl-remove-if #'get-buffer-window (or buffer-list (enfer-buffer-list))))

;;;###autoload
(defun enfer-matching-buffers (pattern &optional buffer-list)
  "Get a list of all buffers that match the regex PATTERN."
  (cl-loop for buf in (or buffer-list (enfer-buffer-list))
           when (string-match-p pattern (buffer-name buf))
           collect buf))

;;;###autoload
(defun enfer-set-buffer-real (buffer flag)
  "Forcibly mark BUFFER as FLAG (non-nil = real)."
  (with-current-buffer buffer
    (setq enfer-real-buffer-p flag)))

;;;###autoload
(defun enfer-kill-buffer-and-windows (buffer)
  "Kill the buffer and delete all the windows it's displayed in."
  (dolist (window (get-buffer-window-list buffer))
    (unless (one-window-p t)
      (delete-window window)))
  (kill-buffer buffer))

;;;###autoload
(defun enfer-fixup-windows (windows)
  "Ensure that each of WINDOWS is showing a real buffer or the fallback buffer."
  (dolist (window windows)
    (with-selected-window window
      (when (enfer-unreal-buffer-p (window-buffer))
        (previous-buffer)
        (when (enfer-unreal-buffer-p (window-buffer))
          (switch-to-buffer (enfer-fallback-buffer)))))))

;;;###autoload
(defun enfer-kill-buffer-fixup-windows (buffer)
  "Kill the BUFFER and ensure all the windows it was displayed in have switched
to a real buffer or the fallback buffer."
  (let ((windows (get-buffer-window-list buffer)))
    (kill-buffer buffer)
    (enfer-fixup-windows (cl-remove-if-not #'window-live-p windows))))

;;;###autoload
(defun enfer-kill-buffers-fixup-windows (buffers)
  "Kill the BUFFERS and ensure all the windows they were displayed in have
switched to a real buffer or the fallback buffer."
  (let ((seen-windows (make-hash-table :test 'eq :size 8)))
    (dolist (buffer buffers)
      (let ((windows (get-buffer-window-list buffer)))
        (kill-buffer buffer)
        (dolist (window (cl-remove-if-not #'window-live-p windows))
          (puthash window t seen-windows))))
    (enfer-fixup-windows (hash-table-keys seen-windows))))

;;;###autoload
(defun enfer-kill-matching-buffers (pattern &optional buffer-list)
  "Kill all buffers (in current workspace OR in BUFFER-LIST) that match the
regex PATTERN. Returns the number of killed buffers."
  (let ((buffers (enfer-matching-buffers pattern buffer-list)))
    (dolist (buf buffers (length buffers))
      (kill-buffer buf))))


;;
;; Hooks

;;;###autoload
(defun enfer|mark-buffer-as-real ()
  "Hook function that marks the current buffer as real."
  (enfer-set-buffer-real (current-buffer) t))


;;
;; Advice

;;;###autoload
(defun enfer*switch-to-fallback-buffer-maybe (orig-fn)
  "Advice for `kill-current-buffer'. If in a dedicated window, delete it. If there
are no real buffers left OR if all remaining buffers are visible in other
windows, switch to `enfer-fallback-buffer'. Otherwise, delegate to original
`kill-current-buffer'."
  (let ((buf (current-buffer)))
    (cond ((window-dedicated-p)
           (delete-window))
          ((eq buf (enfer-fallback-buffer))
           (message "Can't kill the fallback buffer."))
          ((enfer-real-buffer-p buf)
           (if (and buffer-file-name
                    (buffer-modified-p buf)
                    (not (y-or-n-p
                          (format "Buffer %s is modified; kill anyway?" buf))))
               (message "Aborted")
             (set-buffer-modified-p nil)
             (let (buffer-list-update-hook)
               (when (or ;; if there aren't more real buffers than visible buffers,
                      ;; then there are no real, non-visible buffers left.
                      (not (cl-set-difference (enfer-real-buffer-list)
                                              (enfer-visible-buffers)))
                      ;; if we end up back where we start (or previous-buffer
                      ;; returns nil), we have nowhere left to go
                      (memq (switch-to-prev-buffer nil t) (list buf 'nil)))
                 (switch-to-buffer (enfer-fallback-buffer)))
               (unless (delq (selected-window) (get-buffer-window-list buf nil t))
                 (kill-buffer buf)))))
          ((funcall orig-fn)))))


;;
;; Interactive commands

;;;###autoload
(defun enfer/kill-this-buffer-in-all-windows (buffer &optional dont-save)
  "Kill BUFFER globally and ensure all windows previously showing this buffer
have switched to a real buffer or the fallback buffer.

If DONT-SAVE, don't prompt to save modified buffers (discarding their changes)."
  (interactive
   (list (current-buffer) current-prefix-arg))
  (cl-assert (bufferp buffer) t)
  (when (and (buffer-modified-p buffer) dont-save)
    (with-current-buffer buffer
      (set-buffer-modified-p nil)))
  (enfer-kill-buffer-fixup-windows buffer))

;;;###autoload
(defun enfer/kill-all-buffers (&optional project-p)
  "Kill all buffers and closes their windows.

If PROJECT-P (universal argument), don't close windows and only kill buffers
that belong to the current project."
  (interactive "P")
  (save-some-buffers)
  (unless project-p
    (delete-other-windows))
  (switch-to-buffer (enfer-fallback-buffer))
  (let ((buffers (if project-p (enfer-project-buffer-list) (enfer-buffer-list))))
    (mapc #'kill-buffer buffers)
    (when (called-interactively-p 'interactive)
      (message "Killed %s buffers"
               (- (length buffers)
                  (length (cl-remove-if-not #'buffer-live-p buffers)))))))

;;;###autoload
(defun enfer/kill-other-buffers (&optional project-p)
  "Kill all other buffers (besides the current one).

If PROJECT-P (universal argument), kill only buffers that belong to the current
project."
  (interactive "P")
  (let ((buffers
         (delq (current-buffer)
               (if project-p (enfer-project-buffer-list) (enfer-buffer-list)))))
    (mapc #'enfer-kill-buffer-and-windows buffers)
    (when (called-interactively-p 'interactive)
      (message "Killed %s buffers"
               (- (length buffers)
                  (length (cl-remove-if-not #'buffer-live-p buffers)))))))

;;;###autoload
(defun enfer/kill-matching-buffers (pattern &optional project-p)
  "Kill buffers that match PATTERN in BUFFER-LIST.

If PROJECT-P (universal argument), only kill matching buffers in the current
project."
  (interactive
   (list (read-regexp "Buffer pattern: ")
         current-prefix-arg))
  (let* ((buffers (if project-p (enfer-project-buffer-list) (enfer-buffer-list))))
    (enfer-kill-matching-buffers pattern buffers)
    (when (called-interactively-p 'interactive)
      (message "Killed %d buffer(s)"
               (- (length buffers)
                  (length (cl-remove-if-not #'buffer-live-p buffers)))))))

;;;###autoload
(defun enfer/kill-buried-buffers (&optional project-p)
  "Kill buffers that are buried.

If PROJECT-P (universal argument), only kill buried buffers belonging to the
current project."
  (interactive "P")
  (let ((buffers (enfer-buried-buffers (if project-p (enfer-project-buffer-list)))))
    (mapc #'kill-buffer buffers)
    (when (called-interactively-p 'interactive)
      (message "Killed %d buffer(s)"
               (- (length buffers)
                  (length (cl-remove-if-not #'buffer-live-p buffers)))))))

;;;###autoload
(defun enfer/kill-project-buffers (project)
  "Kill buffers for the specified PROJECT."
  (interactive
   (list (if-let* ((open-projects (enfer-open-projects)))
             (completing-read
              "Kill buffers for project: " open-projects
              nil t nil nil
              (if-let* ((project-root (enfer-project-root))
                        (project-root (abbreviate-file-name project-root))
                        ((member project-root open-projects)))
                  project-root))
           (message "No projects are open!")
           nil)))
  (when project
    (let ((buffers (enfer-project-buffer-list project)))
      (enfer-kill-buffers-fixup-windows buffers)
      (when (called-interactively-p 'interactive)
        (message "Killed %d buffer(s)"
                 (- (length buffers)
                    (length (cl-remove-if-not #'buffer-live-p buffers))))))))
