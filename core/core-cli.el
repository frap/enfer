;;; -*- lexical-binding: t; no-byte-compile: t; -*-

;; Eagerly load these libraries because this module may be loaded in a session
;; that hasn't been fully initialized (where autoloads files haven't been
;; generated or `load-path' populated).
(load! "autoload/debug")
(load! "autoload/files")
(load! "autoload/message")
(load! "autoload/packages")


;;
;; Dispatcher API

(defvar enfer-auto-accept (getenv "YES")
  "If non-nil, Enfer will auto-accept any confirmation prompts during batch
commands like `enfer-packages-install', `enfer-packages-update' and
`enfer-packages-autoremove'.")

(defconst enfer--dispatch-command-alist ())
(defconst enfer--dispatch-alias-alist ())

(defun enfer--dispatch-format (desc &optional short)
  (with-temp-buffer
    (let ((fill-column 72))
      (insert desc)
      (goto-char (point-min))
      (while (re-search-forward "\n\n[^ \n]" nil t)
        (fill-paragraph)))
    (if (not short)
        (buffer-string)
      (goto-char (point-min))
      (buffer-substring-no-properties
       (line-beginning-position)
       (line-end-position)))))

(defun enfer--dispatch-help (&optional command desc &rest args)
  "Display help documentation for a dispatcher command. If COMMAND and DESC are
omitted, show all available commands, their aliases and brief descriptions."
  (if command
      (princ (enfer--dispatch-format desc))
    (print! (bold "%-10s\t%s\t%s" "Command:" "Alias" "Description"))
    (dolist (spec (cl-sort enfer--dispatch-command-alist #'string-lessp
                           :key #'car))
      (cl-destructuring-bind (command &key desc _body) spec
        (let ((aliases (cl-loop for (alias . cmd) in enfer--dispatch-alias-alist
                                if (eq cmd command)
                                collect (symbol-name alias))))
          (print! "  %-10s\t%s\t%s"
                  command (if aliases (string-join aliases ",") "")
                  (enfer--dispatch-format desc t)))))))

(defun enfer-dispatch (cmd args &optional show-help)
  "Parses ARGS and invokes a dispatcher.

If SHOW-HELP is non-nil, show the documentation for said dispatcher."
  (when (equal cmd "help")
    (setq show-help t)
    (when args
      (setq cmd  (car args)
            args (cdr args))))
  (cl-destructuring-bind (command &key desc body)
      (let ((sym (intern cmd)))
        (or (assq sym enfer--dispatch-command-alist)
            (assq (cdr (assq sym enfer--dispatch-alias-alist))
                  enfer--dispatch-command-alist)
            (user-error "Invalid command: %s" sym)))
    (if show-help
        (apply #'enfer--dispatch-help command desc args)
      (funcall body args))))

(defmacro dispatcher! (command form &optional docstring)
  "Define a dispatcher command. COMMAND is a symbol or a list of symbols
representing the aliases for this command. DESC is a string description. The
first line should be short (under 60 letters), as it will be displayed for
bin/enfer help.

BODY will be run when this dispatcher is called."
  (declare (indent defun) (doc-string 3))
  (cl-destructuring-bind (cmd &rest aliases)
      (enfer-enlist command)
    (macroexp-progn
     (append
      (when aliases
        `((dolist (alias ',aliases)
            (setf (alist-get alias enfer--dispatch-alias-alist) ',cmd))))
      `((setf (alist-get ',cmd enfer--dispatch-command-alist)
              (list :desc ,docstring
                    :body (lambda (args) (ignore args) ,form))))))))


;;
;; Dummy dispatch commands

;; These are handled by bin/enfer, except we still want 'help CMD' to print out
;; documentation for them, so...

(dispatcher! run :noop
  "Run Enfer Emacs from bin/enfer's parent directory.

All arguments are passed on to Emacs (except for -p and -e).

  enfer run
  enfer run -nw init.el

WARNING: this command exists for convenience and testing. Enfer will suffer
additional overhead by being started this way. For the best performance, it is
best to run Enfer out of ~/.emacs.d and ~/.enfer.d.")

(dispatcher! (doctor doc) :noop
  "Checks for issues with your environment & Enfer config.

Use the doctor to diagnose common problems or list missing dependencies in
enabled modules.")

(dispatcher! (help h) :noop
  "Look up additional information about a command.")


;;
;; Real dispatch commands

(load! "cli/autoloads")
(load! "cli/byte-compile")
(load! "cli/debug")
(load! "cli/env")
(load! "cli/packages")
(load! "cli/patch-macos")
(load! "cli/quickstart")
(load! "cli/upgrade")
(load! "cli/test")


;;
(defun enfer-refresh (&optional force-p)
  "Ensure Enfer is in a working state by checking autoloads and packages, and
recompiling any changed compiled files. This is the shotgun solution to most
problems with enfer."
  (when (getenv "ENFERENV")
    (enfer-reload-env-file 'force))
  (enfer-reload-enfer-autoloads force-p)
  (unwind-protect
      (progn
        (ignore-errors
          (enfer-packages-autoremove enfer-auto-accept))
        (ignore-errors
          (enfer-packages-install enfer-auto-accept)))
    (enfer-reload-package-autoloads force-p)
    (enfer-byte-compile nil 'recompile)))

(dispatcher! (refresh re) (enfer-refresh 'force)
  "Refresh Enfer.

This is the equivalent of running autoremove, install, autoloads, then
recompile. Run this whenever you:

  1. Modify your `enfer!' block,
  2. Add or remove `package!' blocks to your config,
  3. Add or remove autoloaded functions in module autoloaded files.
  4. Update Enfer outside of Enfer (e.g. with git)

It will ensure that unneeded packages are removed, all needed packages are
installed, autoloads files are up-to-date and no byte-compiled files have gone
stale.")

(provide 'core-cli)
;;; core-cli.el ends here
