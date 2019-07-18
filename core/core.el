;;; core.el --- the heart of the beast -*- lexical-binding: t; -*-

(eval-when-compile
  (and (version< emacs-version "25.3")
       (error "Detected Emacs %s. Enfer only supports Emacs 25.3 and higher"
              emacs-version)))

(defvar enfer-debug-mode (or (getenv "DEBUG") init-file-debug)
  "If non-nil, Enfer will log more.

Use `enfer/toggle-debug-mode' to toggle it. The --debug-init flag and setting the
DEBUG envvar will enable this at startup.")


;;
;;; Constants

(defconst enfer-version "0.0.7"
  "Current version of Enfer Emacs.")

(defconst EMACS26+ (> emacs-major-version 25))
(defconst EMACS27+ (> emacs-major-version 26))

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))


;;
(defvar enfer-emacs-dir
  (eval-when-compile (file-truename user-emacs-directory))
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")

(defvar enfer-core-dir (concat enfer-emacs-dir "core/")
  "The root directory of Enfer's core files. Must end with a slash.")

(defvar enfer-modules-dir (concat enfer-emacs-dir "modules/")
  "The root directory for Enfer's modules. Must end with a slash.")

(defvar enfer-local-dir (concat enfer-emacs-dir ".local/")
  "Root directory for local storage.

Use this as a storage location for this system's installation of Enfer Emacs.
These files should not be shared across systems. By default, it is used by
`enfer-etc-dir' and `enfer-cache-dir'. Must end with a slash.")

(defvar enfer-etc-dir (concat enfer-local-dir "etc/")
  "Directory for non-volatile local storage.

Use this for files that don't change much, like server binaries, external
dependencies or long-term shared data. Must end with a slash.")

(defvar enfer-cache-dir (concat enfer-local-dir "cache/")
  "Directory for volatile local storage.

Use this for files that change often, like cache files. Must end with a slash.")

(defvar enfer-packages-dir (concat enfer-local-dir "packages/")
  "Where package.el and quelpa plugins (and their caches) are stored.

Must end with a slash.")

(defvar enfer-docs-dir (concat enfer-emacs-dir "docs/")
  "Where Enfer's documentation files are stored. Must end with a slash.")

(defvar enfer-private-dir
  (or (getenv "ENFERDIR")
      (let ((xdg-path
             (expand-file-name "enfer/"
                               (or (getenv "XDG_CONFIG_HOME")
                                   "~/.config"))))
        (if (file-directory-p xdg-path) xdg-path))
      "~/.enfer.d/")
  "Where your private configuration is placed.

Defaults to ~/.config/enfer, ~/.enfer.d or the value of the ENFERDIR envvar;
whichever is found first. Must end in a slash.")

(defvar enfer-autoload-file (concat enfer-local-dir "autoloads.el")
  "Where `enfer-reload-enfer-autoloads' stores its core autoloads.

This file is responsible for informing Emacs where to find all of Enfer's
autoloaded core functions (in core/autoload/*.el).")

(defvar enfer-package-autoload-file (concat enfer-local-dir "autoloads.pkg.el")
  "Where `enfer-reload-package-autoloads' stores its package.el autoloads.

This file is compiled from the autoloads files of all installed packages
combined.")

(defvar enfer-env-file (concat enfer-local-dir "env")
  "The location of your envvar file, generated by `enfer env refresh`.

This file contains environment variables scraped from your shell environment,
which is loaded at startup (if it exists). This is helpful if Emacs can't
\(easily) be launched from the correct shell session (particularly for MacOS
users).")


;;
;;; Enfer core variables

(defvar enfer-init-p nil
  "Non-nil if Enfer has been initialized.")

(defvar enfer-init-time nil
  "The time it took, in seconds, for Enfer Emacs to initialize.")

(defvar enfer-emacs-changed-p nil
  "If non-nil, the running version of Emacs is different from the first time
Enfer was setup, which may cause problems.")

(defvar enfer-site-load-path (cons enfer-core-dir load-path)
  "The initial value of `load-path', before it was altered by
`enfer-initialize'.")

(defvar enfer-site-process-environment process-environment
  "The initial value of `process-environment', before it was altered by
`enfer-initialize'.")

(defvar enfer-site-exec-path exec-path
  "The initial value of `exec-path', before it was altered by
`enfer-initialize'.")

(defvar enfer-site-shell-file-name shell-file-name
  "The initial value of `shell-file-name', before it was altered by
`enfer-initialize'.")

(defvar enfer--last-emacs-file (concat enfer-local-dir "emacs-version.el"))
(defvar enfer--last-emacs-version nil)
(defvar enfer--refreshed-p nil)
(defvar enfer--stage 'init)


;;
;;; Custom error types

(define-error 'enfer-error "Erreur dans Enfer Emacs core")
(define-error 'enfer-hook-error "Erreur dans un Enfer startup hook" 'enfer-error)
(define-error 'enfer-autoload-error "Erreur dans un autoloads file" 'enfer-error)
(define-error 'enfer-module-error "Erreur dans un Enfer module" 'enfer-error)
(define-error 'enfer-private-error "Erreur dans private config" 'enfer-error)
(define-error 'enfer-package-error "Erreur avec packages" 'enfer-error)


;;
;;; Custom hooks

(defvar enfer-reload-hook nil
  "A list of hooks to run when `enfer/reload' is called.")


;;
;;; Emacs core configuration

;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))     ; pretty
(prefer-coding-system 'utf-8)          ; pretty
(setq locale-coding-system 'utf-8)     ; please
(unless IS-WINDOWS
  (setq selection-coding-system 'utf-8))  ; with sugar on top

(setq-default
 ad-redefinition-action 'accept   ; silence redefined function warnings
 apropos-do-all t                 ; make `apropos' more useful
 auto-mode-case-fold nil
 autoload-compute-prefixes nil
 debug-on-error enfer-debug-mode
 jka-compr-verbose enfer-debug-mode ; silence compression messages
 ffap-machine-p-known 'reject     ; don't ping things that look like domain names
 find-file-visit-truename t       ; resolve symlinks when opening files
 idle-update-delay 1              ; update ui slightly less often
 ;; be quiet at startup; don't load or display anything unnecessary
 inhibit-startup-message t
 inhibit-startup-echo-area-message user-login-name
 inhibit-default-init t
 initial-major-mode 'fundamental-mode
 initial-scratch-message nil
 ;; History & backup settings (save nothing, that's what git is for)
 auto-save-default nil
 create-lockfiles nil
 history-length 500
 make-backup-files nil  ; don't create backup~ files
 ;; byte compilation
 byte-compile-verbose enfer-debug-mode
 byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)
 ;; security
 gnutls-verify-error (not (getenv "INSECURE")) ; you shouldn't use this
 tls-checktrust gnutls-verify-error
 tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
                   ;; compatibility fallbacks
                   "gnutls-cli -p %p %h"
                   "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof")
 ;; Don't store authinfo in plain text!
 auth-sources (list (expand-file-name "authinfo.gpg" enfer-etc-dir)
                    "~/.authinfo.gpg")
 ;; Don't litter `enfer-emacs-dir'
 abbrev-file-name             (concat enfer-local-dir "abbrev.el")
 async-byte-compile-log-file  (concat enfer-etc-dir "async-bytecomp.log")
 auto-save-list-file-name     (concat enfer-cache-dir "autosave")
 backup-directory-alist       (list (cons "." (concat enfer-cache-dir "backup/")))
 custom-file                  (concat enfer-private-dir "init.el")
 desktop-dirname              (concat enfer-etc-dir "desktop")
 desktop-base-file-name       "autosave"
 desktop-base-lock-name       "autosave-lock"
 pcache-directory             (concat enfer-cache-dir "pcache/")
 request-storage-directory    (concat enfer-cache-dir "request")
 server-auth-dir              (concat enfer-cache-dir "server/")
 shared-game-score-directory  (concat enfer-etc-dir "shared-game-score/")
 tramp-auto-save-directory    (concat enfer-cache-dir "tramp-auto-save/")
 tramp-backup-directory-alist backup-directory-alist
 tramp-persistency-file-name  (concat enfer-cache-dir "tramp-persistency.el")
 url-cache-directory          (concat enfer-cache-dir "url/")
 url-configuration-directory  (concat enfer-etc-dir "url/")
 gamegrid-user-score-file-directory (concat enfer-etc-dir "games/"))

(defun enfer*symbol-file (orig-fn symbol &optional type)
  "If a `enfer-file' symbol property exists on SYMBOL, use that instead of the
original value of `symbol-file'."
  (or (if (symbolp symbol) (get symbol 'enfer-file))
      (funcall orig-fn symbol type)))
(advice-add #'symbol-file :around #'enfer*symbol-file)


;;
;;; Minor mode version of `auto-mode-alist'

(defvar enfer-auto-minor-mode-alist '()
  "Alist mapping filename patterns to corresponding minor mode functions, like
`auto-mode-alist'. All elements of this alist are checked, meaning you can
enable multiple minor modes for the same regexp.")

(defun enfer|enable-minor-mode-maybe ()
  "Check file name against `enfer-auto-minor-mode-alist'."
  (when (and buffer-file-name enfer-auto-minor-mode-alist)
    (let ((name buffer-file-name)
          (remote-id (file-remote-p buffer-file-name))
          (alist enfer-auto-minor-mode-alist))
      ;; Remove backup-suffixes from file name.
      (setq name (file-name-sans-versions name))
      ;; Remove remote file name identification.
      (when (and (stringp remote-id)
                 (string-match (regexp-quote remote-id) name))
        (setq name (substring name (match-end 0))))
      (while (and alist (caar alist) (cdar alist))
        (if (string-match-p (caar alist) name)
            (funcall (cdar alist) 1))
        (setq alist (cdr alist))))))
(add-hook 'find-file-hook #'enfer|enable-minor-mode-maybe)


;;
;;; MODE-local-vars-hook

;; File+dir local variables are initialized after the major mode and its hooks
;; have run. If you want hook functions to be aware of these customizations, add
;; them to MODE-local-vars-hook instead.
(defun enfer|run-local-var-hooks ()
  "Run MODE-local-vars-hook after local variables are initialized."
  (run-hook-wrapped (intern-soft (format "%s-local-vars-hook" major-mode))
                    #'enfer-try-run-hook))
(add-hook 'hack-local-variables-hook #'enfer|run-local-var-hooks)

;; If `enable-local-variables' is disabled, then `hack-local-variables-hook' is
;; never triggered.
(defun enfer|run-local-var-hooks-if-necessary ()
  "Run `enfer|run-local-var-hooks' if `enable-local-variables' is disabled."
  (unless enable-local-variables
    (enfer|run-local-var-hooks)))
(add-hook 'after-change-major-mode-hook #'enfer|run-local-var-hooks-if-necessary 'append)

(defun enfer|create-non-existent-directories ()
  "Automatically create missing directories when creating new files."
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))
(add-hook 'find-file-not-found-functions #'enfer|create-non-existent-directories)


;;
;;; Incremental lazy-loading

(defvar enfer-incremental-packages '(t)
  "A list of packages to load incrementally after startup. Any large packages
here may cause noticable pauses, so it's recommended you break them up into
sub-packages. For example, `org' is comprised of many packages, and can be
broken up into:

  (enfer-load-packages-incrementally
   '(calendar find-func format-spec org-macs org-compat
     org-faces org-entities org-list org-pcomplete org-src
     org-footnote org-macro ob org org-clock org-agenda
     org-capture))

This is already done by the lang/org module, however.

If you want to disable incremental loading altogether, either remove
`enfer|load-packages-incrementally' from `emacs-startup-hook' or set
`enfer-incremental-first-idle-timer' to nil.")

(defvar enfer-incremental-first-idle-timer 2
  "How long (in idle seconds) until incremental loading starts.

Set this to nil to disable incremental loading.")

(defvar enfer-incremental-idle-timer 1.5
  "How long (in idle seconds) in between incrementally loading packages.")

(defun enfer-load-packages-incrementally (packages &optional now)
  "Registers PACKAGES to be loaded incrementally.

If NOW is non-nil, load PACKAGES incrementally, in `enfer-incremental-idle-timer'
intervals."
  (if (not now)
      (nconc enfer-incremental-packages packages)
    (when packages
      (let ((gc-cons-threshold enfer-gc-cons-upper-limit)
            (reqs (cl-delete-if #'featurep packages))
            file-name-handler-alist)
        (when-let (req (if reqs (ignore-errors (pop reqs))))
          (enfer-log "Incrementally loading %s" req)
          (condition-case e
              (or (while-no-input (require req nil t) t)
                  (push req reqs))
            ((error debug)
             (message "Failed to load '%s' package incrementally, because: %s"
                      req e)))
          (if reqs
              (run-with-idle-timer enfer-incremental-idle-timer
                                   nil #'enfer-load-packages-incrementally
                                   reqs t)
            (enfer-log "Finished incremental loading")))))))

(defun enfer|load-packages-incrementally ()
  "Begin incrementally loading packages in `enfer-incremental-packages'.

If this is a daemon session, load them all immediately instead."
  (if (daemonp)
      (mapc #'require (cdr enfer-incremental-packages))
    (when (integerp enfer-incremental-first-idle-timer)
      (run-with-idle-timer enfer-incremental-first-idle-timer
                           nil #'enfer-load-packages-incrementally
                           (cdr enfer-incremental-packages) t))))

(add-hook 'window-setup-hook #'enfer|load-packages-incrementally)


;;
;;; Bootstrap helpers

(defun enfer-try-run-hook (hook)
  "Run HOOK (a hook function), but handle errors better, to make debugging
issues easier.

Meant to be used with `run-hook-wrapped'."
  (enfer-log "Exécuter une enfer hook: %s" hook)
  (condition-case e
      (funcall hook)
    ((debug error)
     (signal 'enfer-hook-error (list hook e))))
  ;; return nil so `run-hook-wrapped' won't short circuit
  nil)

(defun enfer-ensure-same-emacs-version-p ()
  "Check if the running version of Emacs has changed and set
`enfer-emacs-changed-p' if it has."
  (if (load enfer--last-emacs-file 'noerror 'nomessage 'nosuffix)
      (setq enfer-emacs-changed-p
            (not (equal emacs-version enfer--last-emacs-version)))
    (with-temp-file enfer--last-emacs-file
      (princ `(setq enfer--last-emacs-version ,(prin1-to-string emacs-version))
             (current-buffer))))
  (cond ((not enfer-emacs-changed-p))
        ((y-or-n-p
          (format
           (concat "Your version of Emacs has changed from %s to %s, which may cause incompatibility\n"
                   "issues. If you run into errors, run `bin/enfer compile :plugins` or reinstall your\n"
                   "plugins to resolve them.\n\n"
                   "Continue?")
           enfer--last-emacs-version
           emacs-version))
         (delete-file enfer--last-emacs-file))
        (noninteractive (error "Abandonner!"))
        ((kill-emacs))))

(defun enfer-ensure-core-directories-exist ()
  "Make sure all Enfer's essential local directories (in and including
`enfer-local-dir') exist."
  (dolist (dir (list enfer-local-dir enfer-etc-dir enfer-cache-dir enfer-packages-dir))
    (unless (file-directory-p dir)
      (make-directory dir t))))

(defun enfer|display-benchmark (&optional return-p)
  "Display a benchmark, showing number of packages and modules, and how quickly
they were loaded at startup.

If RETURN-P, return the message as a string instead of displaying it."
  (funcall (if return-p #'format #'message)
           "Enfer chargée %s pacquets sur %d modules dans %.03fs"
           (length package-activated-list)
           (if enfer-modules (hash-table-count enfer-modules) 0)
           (or enfer-init-time
               (setq enfer-init-time (float-time (time-subtract (current-time) before-init-time))))))

(defun enfer|run-all-startup-hooks ()
  "Run all startup Emacs hooks. Meant to be executed after starting Emacs with
-q or -Q, for example:

  emacs -Q -l init.el -f enfer|run-all-startup-hooks"
  (run-hook-wrapped 'after-init-hook #'enfer-try-run-hook)
  (setq after-init-time (current-time))
  (dolist (hook (list 'delayed-warnings-hook
                      'emacs-startup-hook 'term-setup-hook
                      'window-setup-hook))
    (run-hook-wrapped hook #'enfer-try-run-hook)))

(defun enfer-initialize-autoloads (file)
  "Tries to load FILE (an autoloads file). Return t on success, throws an error
in interactive sessions, nil otherwise (but logs a warning)."
  (condition-case e
      (load (file-name-sans-extension file) 'noerror 'nomessage)
    ((debug error)
     (if noninteractive
         (message "Autoload avertissement de fichier: %s -> %s" (car e) (error-message-string e))
       (signal 'enfer-autoload-error (list (file-name-nondirectory file) e))))))

(defun enfer-load-env-vars (file)
  "Read and set envvars in FILE."
  (if (not (file-readable-p file))
      (enfer-log "Couldn't read %S envvar file" file)
    (with-temp-buffer
      (insert-file-contents-literally file)
      (search-forward "\n\n" nil t)
      (while (re-search-forward "\n\\([^= \n]+\\)=" nil t)
        (save-excursion
          (let ((var (match-string 1))
                (value (buffer-substring-no-properties
                        (point)
                        (1- (or (when (re-search-forward "^\\([^= ]+\\)=" nil t)
                                  (line-beginning-position))
                                (point-max))))))
            (setenv var value)))))
    (setq exec-path (append (split-string (getenv "PATH")
                                          (if IS-WINDOWS ";" ":"))
                            (list exec-directory))
          shell-file-name (or (getenv "SHELL")
                              shell-file-name))
    t))

(defun enfer-initialize (&optional force-p)
  "Bootstrap Enfer, if it hasn't already (or if FORCE-P is non-nil).

The bootstrap process involves making sure 1) the essential directories exist,
2) the core packages are installed, 3) `enfer-autoload-file' and
`enfer-package-autoload-file' exist and have been loaded, and 4) Enfer's core
files are loaded.

If the cache exists, much of this function isn't run, which substantially
reduces startup time.

The overall load order of Enfer is as follows:

  ~/.emacs.d/init.el
  ~/.emacs.d/core/core.el
  ~/.enfer.d/init.el
  Module init.el files
  `enfer-before-init-modules-hook'
  Module config.el files
  ~/.enfer.d/config.el
  `enfer-init-modules-hook'
  `after-init-hook'
  `emacs-startup-hook'
  `enfer-init-ui-hook'
  `window-setup-hook'

Module load order is determined by your `enfer!' block. See `enfer-modules-dirs'
for a list of all recognized module trees. Order defines precedence (from most
to least)."
  (add-to-list 'load-path enfer-core-dir)
  (require 'core-lib)

  (when (or force-p (not enfer-init-p))
    (setq enfer-init-p t)  ; Prevent infinite recursion

    ;; Reset as much state as possible
    (setq exec-path enfer-site-exec-path
          load-path enfer-site-load-path
          process-environment enfer-site-process-environment
          shell-file-name enfer-site-shell-file-name)

    ;; `enfer-autoload-file' tells Emacs where to load all its autoloaded
    ;; functions from. This includes everything in core/autoload/*.el and all
    ;; the autoload files in your enabled modules.
    (when (or force-p (not (enfer-initialize-autoloads enfer-autoload-file)))
      (enfer-ensure-core-directories-exist)
      (enfer-ensure-same-emacs-version-p)

      (require 'core-packages)
      (enfer-ensure-packages-initialized force-p)
      (enfer-ensure-core-packages)

      (unless (or force-p noninteractive)
        (user-error "Votre enfer autoloads sont manquantes! Exécuter `bin/enfer refresh' pour les régénérer")))

    ;; Loads `enfer-package-autoload-file', which loads a concatenated package
    ;; autoloads file and caches `load-path', `auto-mode-alist',
    ;; `Info-directory-list', `enfer-disabled-packages' and
    ;; `package-activated-list'. A big reduction in startup time.
    (let (command-switch-alist)
      (unless (or force-p
                  (enfer-initialize-autoloads enfer-package-autoload-file)
                  noninteractive)
        (user-error "Votre package autoloads sont manquantes! Exécuter `bin/enfer refresh' pour les régénérer")))

    ;; Load shell environment
    (unless noninteractive
      (enfer-load-env-vars enfer-env-file)))

  (require 'core-modules)
  (require 'core-os)
  (if noninteractive
      (require 'core-cli)
    (add-hook 'window-setup-hook #'enfer|display-benchmark)
    (require 'core-keybinds)
    (require 'core-ui)
    (require 'core-projects)
    (require 'core-editor)
    ))


;;
;;; Bootstrap Enfer

(enfer-initialize noninteractive)
(unless noninteractive
  (enfer-initialize-modules))
(with-eval-after-load 'package
  (require 'core-packages)
  (enfer-initialize-packages))

(provide 'core)
;;; core.el ends here
