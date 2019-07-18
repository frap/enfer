;;; core/cli/autoloads.el -*- lexical-binding: t; -*-

(dispatcher! (autoloads a)
  (enfer-reload-autoloads nil 'force)
  "Regenerates Enfer's autoloads files.

It scans and reads autoload cookies (;;;###autoload) in core/autoload/*.el,
modules/*/*/autoload.el and modules/*/*/autoload/*.el, and generates and
byte-compiles `enfer-autoload-file', as well as `enfer-package-autoload-file'
(created from the concatenated autoloads files of all installed packages).

It also caches `load-path', `Info-directory-list', `enfer-disabled-packages',
`package-activated-list' and `auto-mode-alist'.")

;; external variables
(defvar autoload-timestamps)
(defvar generated-autoload-load-name)
(defvar generated-autoload-file)


;;
;;; Helpers

(defvar enfer-autoload-excluded-packages '(marshal gh)
  "Packages that have silly or destructive autoload files that try to load
everyone in the universe and their dog, causing errors that make babies cry. No
one wants that.")

(defun enfer-delete-autoloads-file (file)
  "Delete FILE (an autoloads file) and accompanying *.elc file, if any."
  (cl-check-type file string)
  (when (file-exists-p file)
    (when-let (buf (find-buffer-visiting enfer-autoload-file))
      (with-current-buffer buf
        (set-buffer-modified-p nil))
      (kill-buffer buf))
    (delete-file file)
    (ignore-errors (delete-file (byte-compile-dest-file file)))
    (message "Deleted old %s" (file-name-nondirectory file))))

(defun enfer--warn-refresh-session ()
  (print! (bold (green "\nFinished!")))
  (message "If you have a running Emacs Session, you will need to restart it or")
  (message "reload Enfer for changes to take effect:\n")
  (message "  M-x enfer/restart-and-restore")
  (message "  M-x enfer/restart")
  (message "  M-x enfer/reload"))

(defun enfer--reload-files (&rest files)
  (if (not noninteractive)
      (dolist (file files)
        (load-file (byte-compile-dest-file file)))
    (add-hook 'kill-emacs-hook #'enfer--warn-refresh-session)))

(defun enfer--byte-compile-file (file)
  (let ((short-name (file-name-nondirectory file))
        (byte-compile-dynamic-docstrings t))
    (condition-case e
        (when (byte-compile-file file)
          ;; Give autoloads file a chance to report error
          (load (if enfer-debug-mode
                    file
                  (byte-compile-dest-file file))
                nil t)
          (unless noninteractive
            (message "Compilation terminée %s" short-name)))
      ((debug error)
       (let ((backup-file (concat file ".bk")))
         (message "Copie de sauvegarde sur %s" backup-file)
         (copy-file file backup-file 'overwrite))
       (enfer-delete-autoloads-file file)
       (signal 'enfer-autoload-error (list short-name e))))))

(defun enfer-reload-autoloads (&optional file force-p)
  "Reloads FILE (an autoload file), if it needs reloading.

FILE should be one of `enfer-autoload-file' or `enfer-package-autoload-file'. If
it is nil, it will try to reload both. If FORCE-P (universal argument) do it
even if it doesn't need reloading!"
  (or (null file)
      (stringp file)
      (signal 'wrong-type-argument (list 'stringp file)))
  (if (stringp file)
      (cond ((file-equal-p file enfer-autoload-file)
             (enfer-reload-enfer-autoloads force-p))
            ((file-equal-p file enfer-package-autoload-file)
             (enfer-reload-package-autoloads force-p))
            ((error "Invalid autoloads file: %s" file)))
    (enfer-reload-enfer-autoloads force-p)
    (enfer-reload-package-autoloads force-p)))


;;
;;; Enfer autoloads

(defun enfer--file-cookie-p (file)
  "Returns the return value of the ;;;###if predicate form in FILE."
  (with-temp-buffer
    (insert-file-contents-literally file nil 0 256)
    (if (and (re-search-forward "^;;;###if " nil t)
             (<= (line-number-at-pos) 3))
        (let ((load-file-name file))
          (eval (sexp-at-point)))
      t)))

(defun enfer--generate-header (func)
  (goto-char (point-min))
  (insert ";; -*- lexical-binding:t -*-\n"
          ";; This file is autogenerated by `" (symbol-name func) "', DO NOT EDIT !!\n\n"))

(defun enfer--generate-autoloads (targets)
  (require 'autoload)
  (dolist (file targets)
    (let* ((file (file-truename file))
           (generated-autoload-file enfer-autoload-file)
           (generated-autoload-load-name (file-name-sans-extension file))
           (noninteractive (not enfer-debug-mode))
           autoload-timestamps)
      (print!
       (cond ((not (enfer--file-cookie-p file))
              "⚠ Ignoré %s")
             ((autoload-generate-file-autoloads file (current-buffer))
              (yellow "✕ Rien dans %s"))
             ((green "✓ Scanné %s")))
       (if (file-in-directory-p file default-directory)
           (file-relative-name file)
         (abbreviate-file-name file))))))

(defun enfer--expand-autoloads ()
  (let ((load-path
         ;; NOTE With `enfer-private-dir' in `load-path', Enfer autoloads files
         ;; will be unable to declare autoloads for the built-in autoload.el
         ;; Emacs package, should $ENFERDIR/autoload.el exist. Not sure why
         ;; they'd want to though, so it's an acceptable compromise.
         (append (list enfer-private-dir enfer-emacs-dir)
                 enfer-modules-dirs
                 load-path))
        cache)
    (while (re-search-forward "^\\s-*(autoload\\s-+'[^ ]+\\s-+\"\\([^\"]*\\)\"" nil t)
      (let ((path (match-string 1)))
        (replace-match
         (or (cdr (assoc path cache))
             (when-let* ((libpath (locate-library path))
                         (libpath (file-name-sans-extension libpath)))
               (push (cons path (abbreviate-file-name libpath)) cache)
               libpath)
             path)
         t t nil 1)))))

(defun enfer--generate-autodefs (targets enabled-targets)
  (goto-char (point-max))
  (search-backward ";;;***" nil t)
  (save-excursion (insert "\n"))
  (dolist (path targets)
    (insert
     (with-temp-buffer
       (insert-file-contents path)
       (let ((member-p (or (member path enabled-targets)
                           (file-in-directory-p path enfer-core-dir)))
             forms)
         (while (re-search-forward "^;;;###autodef *\\([^\n]+\\)?\n" nil t)
           (let* ((sexp (sexp-at-point))
                  (alt-sexp (match-string 1))
                  (type (car sexp))
                  (name (enfer-unquote (cadr sexp)))
                  (origin (cond ((enfer-module-from-path path))
                                ((file-in-directory-p path enfer-private-dir)
                                 `(:private . ,(intern (file-name-base path))))
                                ((file-in-directory-p path enfer-emacs-dir)
                                 `(:core . ,(intern (file-name-base path))))))
                  (enfer-file-form
                   `(put ',name 'enfer-file ,(abbreviate-file-name path))))
             (cond ((and (not member-p) alt-sexp)
                    (push (read alt-sexp) forms))

                   ((memq type '(defun defmacro cl-defun cl-defmacro))
                    (cl-destructuring-bind (_ name arglist &rest body) sexp
                      (let ((docstring (if (stringp (car body))
                                           (pop body)
                                         "No documentation.")))
                        (push (if member-p
                                  (make-autoload sexp (abbreviate-file-name (file-name-sans-extension path)))
                                (push enfer-file-form forms)
                                (setq docstring (format "THIS FUNCTION DOES NOTHING BECAUSE %s IS DISABLED\n\n%s"
                                                        origin docstring))
                                (condition-case-unless-debug e
                                    (if alt-sexp
                                        (read alt-sexp)
                                      (append (list (pcase type
                                                      (`defun 'defmacro)
                                                      (`cl-defun `cl-defmacro)
                                                      (_ type))
                                                    name arglist docstring)
                                              (cl-loop for arg in arglist
                                                       if (and (symbolp arg)
                                                               (not (keywordp arg))
                                                               (not (memq arg cl--lambda-list-keywords)))
                                                       collect arg into syms
                                                       else if (listp arg)
                                                       collect (car arg) into syms
                                                       finally return (if syms `((ignore ,@syms))))))
                                  ('error
                                   (message "Ignoring autodef %s (%s)"
                                            name e)
                                   nil)))
                              forms)
                        (push `(put ',name 'enfer-module ',origin) forms))))

                   ((eq type 'defalias)
                    (cl-destructuring-bind (_type name target &optional docstring) sexp
                      (let ((name (enfer-unquote name))
                            (target (enfer-unquote target)))
                        (unless member-p
                          (setq docstring (format "THIS FUNCTION DOES NOTHING BECAUSE %s IS DISABLED\n\n%s"
                                                  origin docstring))
                          (setq target #'ignore))
                        (push enfer-file-form forms)
                        (push `(put ',name 'enfer-module ',origin) forms)
                        (push `(defalias ',name #',target ,docstring)
                              forms))))

                   (member-p
                    (push sexp forms)))))
         (if forms
             (concat (mapconcat #'prin1-to-string (nreverse forms) "\n")
                     "\n")
           ""))))))

(defun enfer--cleanup-autoloads ()
  (goto-char (point-min))
  (when (re-search-forward "^;;\\(;[^\n]*\\| no-byte-compile: t\\)\n" nil t)
    (replace-match "" t t)))

(defun enfer-reload-enfer-autoloads (&optional force-p)
  "Refreshes `enfer-autoload-file', if necessary (or if FORCE-P is non-nil).

It scans and reads autoload cookies (;;;###autoload) in core/autoload/*.el,
modules/*/*/autoload.el and modules/*/*/autoload/*.el, and generates
`enfer-autoload-file'.

Run this whenever your `enfer!' block, or a module autoload file, is modified."
  (let* ((default-directory enfer-emacs-dir)
         (enfer-modules (enfer-modules))
         (targets
          (file-expand-wildcards
           (expand-file-name "autoload/*.el" enfer-core-dir)))
         (enabled-targets (copy-sequence targets))
         case-fold-search)
    (dolist (path (enfer-module-load-path t))
      (let* ((auto-dir  (expand-file-name "autoload" path))
             (auto-file (expand-file-name "autoload.el" path))
             (module    (enfer-module-from-path auto-file))
             (module-p  (or (enfer-module-p (car module) (cdr module))
                            (file-equal-p path enfer-private-dir))))
        (when (file-exists-p auto-file)
          (push auto-file targets)
          (if module-p (push auto-file enabled-targets)))
        (dolist (file (enfer-files-in auto-dir :match "\\.el$" :full t :sort nil))
          (push file targets)
          (if module-p (push file enabled-targets)))))
    (if (and (not force-p)
             (not enfer-emacs-changed-p)
             (file-exists-p enfer-autoload-file)
             (not (file-newer-than-file-p (expand-file-name "init.el" enfer-private-dir)
                                          enfer-autoload-file))
             (not (cl-loop for file in targets
                           if (file-newer-than-file-p file enfer-autoload-file)
                           return t)))
        (progn (print! (green "Enfer core autoloads est à jour"))
               (enfer-initialize-autoloads enfer-autoload-file)
               nil)
      (enfer-delete-autoloads-file enfer-autoload-file)
      (message "Générer de nouveaux autoloads.el")
      (make-directory (file-name-directory enfer-autoload-file) t)
      (with-temp-file enfer-autoload-file
        (enfer--generate-header 'enfer-reload-enfer-autoloads)
        (save-excursion
          (enfer--generate-autoloads (reverse enabled-targets)))
          ;; Replace autoload paths (only for module autoloads) with absolute
          ;; paths for faster resolution during load and simpler `load-path'
        (save-excursion
          (enfer--expand-autoloads)
          (print! (green "✓ Expanded module autoload paths")))
        ;; Generates stub definitions for functions/macros defined in disabled
        ;; modules, so that you will never get a void-function when you use
        ;; them.
        (save-excursion
          (enfer--generate-autodefs (reverse targets) enabled-targets)
          (print! (green "✓ Generated autodefs")))
        ;; Remove byte-compile-inhibiting file variables so we can byte-compile
        ;; the file, and autoload comments.
        (enfer--cleanup-autoloads)
        (print! (green "✓ Clean up autoloads")))
      ;; Byte compile it to give the file a chance to reveal errors.
      (enfer--byte-compile-file enfer-autoload-file)
      (enfer--reload-files enfer-autoload-file)
      t)))


;;
;;; Package autoloads

(defun enfer--generate-package-autoloads ()
  "Concatenates package autoload files, let-binds `load-file-name' around
them,and remove unnecessary `provide' statements or blank links.

Skips over packages in `enfer-autoload-excluded-packages'."
  (dolist (spec (enfer-get-package-alist))
    (if-let* ((pkg  (car spec))
              (desc (cdr spec)))
        (unless (memq pkg enfer-autoload-excluded-packages)
          (let ((file (concat (package--autoloads-file-name desc) ".el")))
            (when (file-exists-p file)
              (insert "(let ((load-file-name " (prin1-to-string (abbreviate-file-name file)) "))\n")
              (insert-file-contents file)
              (while (re-search-forward "^\\(?:;;\\(.*\n\\)\\|\n\\|(provide '[^\n]+\\)" nil t)
                (unless (nth 8 (syntax-ppss))
                  (replace-match "" t t)))
              (unless (bolp) (insert "\n"))
              (insert ")\n"))))
      (message "Couldn't find package desc for %s" (car spec)))))

(defun enfer--generate-var-cache ()
  "Print a `setq' form for expensive-to-initialize variables, so we can cache
them in Enfer's autoloads file."
  (enfer-initialize-packages)
  (prin1 `(setq load-path ',load-path
                auto-mode-alist ',auto-mode-alist
                Info-directory-list ',Info-directory-list
                enfer-disabled-packages ',(mapcar #'car (enfer-find-packages :disabled t))
                package-activated-list ',package-activated-list)
         (current-buffer)))

(defun enfer--cleanup-package-autoloads ()
  "Remove (some) forms that modify `load-path' or `auto-mode-alist'.

These variables are cached all at once and at later, so these removed statements
served no purpose but to waste cycles."
  (while (re-search-forward "^\\s-*\\((\\(?:add-to-list\\|\\(?:when\\|if\\) (boundp\\)\\s-+'\\(?:load-path\\|auto-mode-alist\\)\\)" nil t)
    (goto-char (match-beginning 1))
    (kill-sexp)))

(defun enfer-reload-package-autoloads (&optional force-p)
  "Compiles `enfer-package-autoload-file' from the autoloads files of all
installed packages. It also caches `load-path', `Info-directory-list',
`enfer-disabled-packages', `package-activated-list' and `auto-mode-alist'.

Will do nothing if none of your installed packages have been modified. If
FORCE-P (universal argument) is non-nil, regenerate it anyway.

This should be run whenever your `enfer!' block or update your packages."
  (if (and (not force-p)
           (not enfer-emacs-changed-p)
           (file-exists-p enfer-package-autoload-file)
           (not (file-newer-than-file-p enfer-packages-dir enfer-package-autoload-file))
           (not (ignore-errors
                  (cl-loop for key being the hash-keys of (enfer-modules)
                           for path = (enfer-module-path (car key) (cdr key) "packages.el")
                           if (file-newer-than-file-p path enfer-package-autoload-file)
                           return t))))
      (ignore (print! (green "Enfer package autoloads is up-to-date"))
              (enfer-initialize-autoloads enfer-package-autoload-file))
    (let (case-fold-search)
      (enfer-delete-autoloads-file enfer-package-autoload-file)
      (with-temp-file enfer-package-autoload-file
        (enfer--generate-header 'enfer-reload-package-autoloads)
        (save-excursion
          ;; Cache important and expensive-to-initialize state here.
          (enfer--generate-var-cache)
          (print! (green "✓ Cached package state"))
          ;; Concatenate the autoloads of all installed packages.
          (enfer--generate-package-autoloads)
          (print! (green "✓ Package autoloads included")))
        ;; Remove `load-path' and `auto-mode-alist' modifications (most of them,
        ;; at least); they are cached later, so all those membership checks are
        ;; unnecessary overhead.
        (enfer--cleanup-package-autoloads)
        (print! (green "✓ Removed load-path/auto-mode-alist entries"))))
    (enfer--byte-compile-file enfer-package-autoload-file)
    (enfer--reload-files enfer-package-autoload-file)
    t))