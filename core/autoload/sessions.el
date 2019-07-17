;;; core/autoload/sessions.el -*- lexical-binding: t; -*-

;;
;;; Helpers

;;;###autoload
(defun enfer-session-file (&optional name)
  "TODO"
  (cond ((require 'persp-mode nil t)
         (expand-file-name (or name persp-auto-save-fname) persp-save-dir))
        ((require 'desktop nil t)
         (if name
             (expand-file-name name (file-name-directory (desktop-full-file-name)))
           (desktop-full-file-name)))
        ((error "No session backend available"))))

;;;###autoload
(defun enfer-save-session (&optional file)
  "TODO"
  (setq file (expand-file-name (or file (enfer-session-file))))
  (cond ((require 'persp-mode nil t)
         (unless persp-mode (persp-mode +1))
         (setq persp-auto-save-opt 0)
         (persp-save-state-to-file file))
        ((and (require 'frameset nil t)
              (require 'restart-emacs nil t))
         (let ((frameset-filter-alist (append '((client . restart-emacs--record-tty-file))
                                              frameset-filter-alist))
               (desktop-base-file-name (file-name-nondirectory file))
               (desktop-dirname (file-name-directory file))
               (desktop-restore-eager t)
               desktop-file-modtime)
           (make-directory desktop-dirname t)
           ;; Prevents confirmation prompts
           (let ((desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name)))))
             (desktop-save desktop-dirname t))))
        ((error "No session backend to save session with"))))

;;;###autoload
(defun enfer-load-session (&optional file)
  "TODO"
  (setq file (expand-file-name (or file (enfer-session-file))))
  (message "Attempting to load %s" file)
  (cond ((require 'persp-mode nil t)
         (unless persp-mode
           (persp-mode +1))
         (persp-load-state-from-file file))
        ((and (require 'frameset nil t)
              (require 'restart-emacs nil t))
         (restart-emacs--restore-frames-using-desktop file))
        ((error "No session backend to load session with"))))


;;
;;; Command line switch

;;;###autoload
(defun enfer-restore-session-handler (&rest _)
  "TODO"
  (add-hook 'window-setup-hook #'enfer-load-session 'append))

;;;###autoload
(add-to-list 'command-switch-alist (cons "--restore" #'enfer-restore-session-handler))


;;
;;; Commands

;;;###autoload
(defun enfer/quickload-session ()
  "TODO"
  (interactive)
  (message "Restoring session...")
  (enfer-load-session)
  (message "Session restored. Welcome back."))

;;;###autoload
(defun enfer/quicksave-session ()
  "TODO"
  (interactive)
  (message "Saving session")
  (enfer-save-session)
  (message "Saving session...DONE"))

;;;###autoload
(defun enfer/load-session (file)
  "TODO"
  (interactive
   (let ((session-file (enfer-session-file)))
     (list (or (read-file-name "Session to restore: "
                               (file-name-directory session-file)
                               nil t
                               (file-name-nondirectory session-file))
               (user-error "No session selected. Aborting")))))
  (unless file
    (error "No session file selected"))
  (message "Loading '%s' session" file)
  (enfer-load-session file))

;;;###autoload
(defun enfer/save-session (file)
  "TODO"
  (interactive
   (let ((session-file (enfer-session-file)))
     (list (or (read-file-name "Save session to: "
                               (file-name-directory session-file)
                               nil nil
                               (file-name-nondirectory session-file))
               (user-error "No session selected. Aborting")))))
  (unless file
    (error "No session file selected"))
  (message "Saving '%s' session" file)
  (enfer-save-session file))

;;;###autoload
(defalias 'enfer/restart #'restart-emacs)

;;;###autoload
(defun enfer/restart-and-restore (&optional debug)
  "TODO"
  (interactive "P")
  (setq enfer-autosave-session nil)
  (enfer/quicksave-session)
  (restart-emacs
   (delq nil (list (if debug "--debug-init") "--restore"))))
