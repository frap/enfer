;;; core/autoload/config.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar enfer-reloading-p nil
  "TODO")

;;;###autoload
(defun enfer/open-private-config ()
  "Browse your `enfer-private-dir'."
  (interactive)
  (unless (file-directory-p enfer-private-dir)
    (make-directory enfer-private-dir t))
  (enfer-project-browse enfer-private-dir))

;;;###autoload
(defun enfer/find-file-in-private-config ()
  "Search for a file in `enfer-private-dir'."
  (interactive)
  (enfer-project-find-file enfer-private-dir))

;;;###autoload
(defun enfer/reload (&optional force-p)
  "Reloads your private config.

This is experimental! It will try to do as `bin/enfer refresh' does, but from
within this Emacs session. i.e. it reload autoloads files (if necessary),
reloads your package list, and lastly, reloads your private config.el.

Runs `enfer-reload-hook' afterwards."
  (interactive "P")
  (require 'core-cli)
  (general-auto-unbind-keys)
  (let ((enfer-reloading-p t))
    (when (getenv "ENFERENV")
      (enfer-reload-env-file 'force))
    (enfer-reload-autoloads force-p)
    (let (enfer-init-p)
      (enfer-initialize))
    (with-demoted-errors "PRIVATE CONFIG ERROR: %s"
      (let (enfer-init-modules-p)
        (enfer-initialize-modules)))
    (when (bound-and-true-p enfer-packages)
      (enfer/reload-packages))
    (run-hook-wrapped 'enfer-reload-hook #'enfer-try-run-hook))
  (general-auto-unbind-keys t)
  (message "Finished!"))

;;;###autoload
(defun enfer/reload-env ()
  "Regenerates and reloads your shell environment.

Uses the same mechanism as 'bin/enfer env reload'."
  (interactive)
  (compile (format "%s env refresh" (expand-file-name "bin/enfer" enfer-emacs-dir)))
  (while compilation-in-progress
    (sit-for 1))
  (unless (file-readable-p enfer-env-file)
    (error "Failed to generate env file"))
  (enfer-load-env-vars enfer-env-file))

;;;###autoload
(defun enfer/reload-font ()
  "Reload your fonts, if they're set.
See `enfer|init-fonts'."
  (interactive)
  (when enfer-font
    (set-frame-font enfer-font t))
  (enfer|init-fonts)
  (mapc #'enfer|init-emoji-fonts (frame-list)))

;;;###autoload
(defun enfer/reload-theme ()
  "Reload the current color theme."
  (interactive)
  (let ((theme (or (car-safe custom-enabled-themes) enfer-theme)))
    (when theme
      (mapc #'disable-theme custom-enabled-themes))
    (when (and enfer-theme (not (memq enfer-theme custom-enabled-themes)))
      (let (enfer--prefer-theme-elc)
        (load-theme enfer-theme t)))
    (enfer|init-fonts)))
