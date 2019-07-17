;;; core/autoload/help.el -*- lexical-binding: t; -*-

(defvar enfer--help-major-mode-module-alist
  '((dockerfile-mode :tools docker)
    (agda2-mode      :lang agda)
    (haxor-mode      :lang assembly)
    (mips-mode       :lang assembly)
    (nasm-mode       :lang assembly)
    (c-mode          :lang cc)
    (c++-mode        :lang cc)
    (objc++-mode     :lang cc)
    (crystal-mode    :lang crystal)
    (lisp-mode       :lang common-lisp)
    (csharp-mode     :lang csharp)
    (clojure-mode    :lang clojure)
    (graphql-mode    :lang data)
    (toml-mode       :lang data)
    (json-mode       :lang data)
    (yaml-mode       :lang data)
    (csv-mode        :lang data)
    (dhall-mode      :lang data)
    (erlang-mode     :lang erlang)
    (elixir-mode     :lang elixir)
    (elm-mode        :lang elm)
    (emacs-lisp-mode :lang emacs-lisp)
    (ess-r-mode      :lang ess)
    (ess-julia-mode  :lang ess)
    (go-mode         :lang go)
    (haskell-mode    :lang haskell)
    (hy-mode         :lang hy)
    (idris-mode      :lang idris)
    (java-mode       :lang java)
    (js2-mode        :lang javascript)
    (rjsx-mode       :lang javascript)
    (typescript-mode :lang javascript)
    (coffee-mode     :lang javascript)
    (julia-mode      :lang julia)
    (kotlin-mode     :lang kotlin)
    (latex-mode      :lang latex)
    (LaTeX-mode      :lang latex)
    (ledger-mode     :lang ledger)
    (lua-mode        :lang lua)
    (markdown-mode   :lang markdown)
    (gfm-mode        :lang markdown)
    (nim-mode        :lang nim)
    (nix-mode        :lang nix)
    (taureg-mode     :lang ocaml)
    (org-mode        :lang org)
    (perl-mode       :lang perl)
    (php-mode        :lang php)
    (hack-mode       :lang php)
    (plantuml-mode   :lang plantuml)
    (purescript-mode :lang purescript)
    (python-mode     :lang python)
    (restclient-mode :lang rest)
    (ruby-mode       :lang ruby)
    (enh-ruby-mode   :lang ruby)
    (rust-mode       :lang rust)
    (scala-mode      :lang scala)
    (sh-mode         :lang sh)
    (swift-mode      :lang swift)
    (web-mode        :lang web)
    (css-mode        :lang web)
    (scss-mode       :lang web)
    (sass-mode       :lang web)
    (less-css-mode   :lang web)
    (stylus-mode     :lang web)
    (terra-mode      :lang terra)
    (vala-mode       :lang vala))
  "An alist mapping major modes to Enfer modules.

This is used by `enfer/help-modules' to auto-select the module corresponding to
the current major-modea.")


;;
;;; Helpers

;;;###autoload
(defun enfer-active-minor-modes ()
  "Return a list of active minor-mode symbols."
  (cl-loop for mode in minor-mode-list
           if (and (boundp mode) (symbol-value mode))
           collect mode))


;;
;;; Custom describe commands

;;;###autoload (defalias 'enfer/describe-autodefs #'enfer/help-autodefs)
;;;###autoload (defalias 'enfer/describe-module   #'enfer/help-modules)
;;;###autoload (defalias 'enfer/describe-package  #'enfer/help-packages)

;;;###autoload
(defun enfer/describe-active-minor-mode (mode)
  "Get information on an active minor mode. Use `describe-minor-mode' for a
selection of all minor-modes, active or not."
  (interactive
   (list (completing-read "Minor mode: " (enfer-active-minor-modes))))
  (describe-minor-mode-from-symbol
   (cond ((stringp mode) (intern mode))
         ((symbolp mode) mode)
         ((error "Expected a symbol/string, got a %s" (type-of mode))))))

;;;###autoload
(defun enfer/describe-symbol (symbol)
  "Show help for SYMBOL, a variable, function or macro."
  (interactive
   (list (helpful--read-symbol "Symbol: " #'helpful--bound-p)))
  (let* ((sym (intern-soft symbol))
         (bound (boundp sym))
         (fbound (fboundp sym)))
    (cond ((and sym bound (not fbound))
           (helpful-variable sym))
          ((and sym fbound (not bound))
           (helpful-callable sym))
          ((apropos (format "^%s\$" symbol)))
          ((apropos (format "%s" symbol))))))


;;
;;; Documentation commands

(defun enfer--org-headings (files &optional depth include-files)
  "TODO"
  (require 'org)
  (let* ((default-directory enfer-docs-dir)
         (org-agenda-files (mapcar #'expand-file-name (enfer-enlist files)))
         (depth (if (integerp depth) depth)))
    (unwind-protect
        (delq nil
              (org-map-entries
               (lambda ()
                 (cl-destructuring-bind (level _reduced-level _todo _priority text tags)
                     (org-heading-components)
                   (let ((path (org-get-outline-path)))
                     (when (and (or (null depth)
                                    (<= level depth))
                                (or (null tags)
                                    (not (string-match-p ":TOC" tags))))
                       (propertize
                        (mapconcat
                         'identity
                         (list (mapconcat #'identity
                                          (append (when include-files
                                                    (list (or (+org-get-property "TITLE")
                                                              (file-relative-name buffer-file-name))))
                                                  path
                                                  (list (replace-regexp-in-string org-any-link-re "\\4" text)))
                                          " > ")
                               tags)
                         " ")
                        'location (cons buffer-file-name (point)))))))
               t 'agenda))
      (mapc #'kill-buffer org-agenda-new-buffers)
      (setq org-agenda-new-buffers nil))))

;;;###autoload
(defun enfer-completing-read-org-headings (prompt files &optional depth include-files initial-input)
  "TODO"
  (let (ivy-sort-functions-alist)
    (if-let* ((result (completing-read
                       prompt
                       (enfer--org-headings files depth include-files)
                       nil nil initial-input)))
        (cl-destructuring-bind (file . location)
            (get-text-property 0 'location result)
          (find-file file)
          (goto-char location))
      (user-error "Aborted"))))

;;;###autoload
(defun enfer/help ()
  "Open Enfer's user manual."
  (interactive)
  (find-file (expand-file-name "index.org" enfer-docs-dir)))

;;;###autoload
(defun enfer/help-search (&optional initial-input)
  "Search Enfer's documentation and jump to a headline."
  (interactive)
  (enfer-completing-read-org-headings
   "Find in Enfer help: "
   (list "getting_started.org"
         "contributing.org"
         "troubleshooting.org"
         "tutorials.org"
         "faq.org"
         "../modules/README.org")
   2 t initial-input))

;;;###autoload
(defun enfer/help-news-search (&optional initial-input)
  "Search headlines in Enfer's newsletters."
  (interactive)
  (enfer-completing-read-org-headings
   "Find in News: "
   (nreverse (enfer-files-in (expand-file-name "news" enfer-docs-dir)
                            :match "/[0-9]"
                            :relative-to enfer-docs-dir
                            :sort t))
   nil t initial-input))

;;;###autoload
(defun enfer/help-faq (&optional initial-input)
  "Search Enfer's FAQ and jump to a question."
  (interactive)
  (enfer-completing-read-org-headings
   "Find in FAQ: " (list "faq.org")
   nil nil initial-input))

;;;###autoload
(defun enfer/help-news ()
  "Open a Enfer newsletter.
The latest newsletter will be selected by default."
  (interactive)
  (let* ((default-directory (expand-file-name "news/" enfer-docs-dir))
         (news-files (enfer-files-in default-directory)))
    (find-file
     (read-file-name (format "Open Enfer newsletter (current: v%s): "
                             enfer-version)
                     default-directory
                     (if (member enfer-version news-files)
                         enfer-version
                       (concat (mapconcat #'number-to-string
                                          (nbutlast (version-to-list enfer-version) 1)
                                          ".")
                               ".x"))
                     t enfer-version))))

;;;###autoload
(defun enfer/help-autodefs (autodef)
  "Open documentation for an autodef.

An autodef is a Enfer concept. It is a function or macro that is always defined,
whether or not its containing module is disabled (in which case it will safely
no-op without evaluating its arguments). This syntactic sugar lets you use them
without needing to check if they are available."
  (interactive
   (let* ((settings
           (cl-loop with case-fold-search = nil
                    for sym being the symbols of obarray
                    for sym-name = (symbol-name sym)
                    if (and (or (functionp sym)
                                (macrop sym))
                            (string-match-p "[a-z]!$" sym-name))
                    collect sym))
          (sym (symbol-at-point))
          (autodef
           (completing-read
            "Describe setter: "
            ;; TODO Could be cleaner (refactor me!)
            (cl-loop with maxwidth = (apply #'max (mapcar #'length (mapcar #'symbol-name settings)))
                     for def in (sort settings #'string-lessp)
                     if (get def 'enfer-module)
                     collect
                     (format (format "%%-%ds%%s" (+ maxwidth 4))
                             def (propertize (format "%s %s" (car it) (cdr it))
                                             'face 'font-lock-comment-face))
                     else if (and (string-match-p "^set-.+!$" (symbol-name def))
                                  (symbol-file def)
                                  (file-in-directory-p (symbol-file def) enfer-core-dir))
                     collect
                     (format (format "%%-%ds%%s" (+ maxwidth 4))
                             def (propertize (format "core/%s.el" (file-name-sans-extension (file-relative-name (symbol-file def) enfer-core-dir)))
                                             'face 'font-lock-comment-face)))
            nil t
            (when (and (symbolp sym)
                       (string-match-p "!$" (symbol-name sym)))
              (symbol-name sym)))))
     (list (and autodef (car (split-string autodef " "))))))
  (or (stringp autodef)
      (functionp autodef)
      (signal 'wrong-type-argument (list '(stringp functionp) autodef)))
  (let ((fn (if (functionp autodef)
                autodef
              (intern-soft autodef))))
    (or (fboundp fn)
        (error "'%s' is not a valid ENFER autodef" autodef))
    (if (fboundp 'helpful-callable)
        (helpful-callable fn)
      (describe-function fn))))

;;;###autoload
(defun enfer/help-modules (category module)
  "Open the documentation for a Enfer module.

CATEGORY is a keyword and MODULE is a symbol. e.g. :editor and 'evil.

Automatically selects a) the module at point (in private init files), b) the
module derived from a `featurep!' or `require!' call, c) the module that the
current file is in, or d) the module associated with the current major mode (see
`enfer--help-major-mode-module-alist')."
  (interactive
   (let* ((module
           (cond ((and buffer-file-name
                       (eq major-mode 'emacs-lisp-mode)
                       (file-in-directory-p buffer-file-name enfer-private-dir)
                       (save-excursion (goto-char (point-min))
                                       (re-search-forward "^\\s-*(enfer! " nil t))
                       (thing-at-point 'sexp t)))
                 ((save-excursion
                    (require 'smartparens)
                    (ignore-errors
                      (sp-beginning-of-sexp)
                      (unless (eq (char-after) ?\()
                        (backward-char))
                      (let ((sexp (sexp-at-point)))
                        (when (memq (car-safe sexp) '(featurep! require!))
                          (format "%s %s" (nth 1 sexp) (nth 2 sexp)))))))
                 ((and buffer-file-name
                       (when-let (mod (enfer-module-from-path buffer-file-name))
                         (format "%s %s" (car mod) (cdr mod)))))
                 ((when-let (mod (cdr (assq major-mode enfer--help-major-mode-module-alist)))
                    (format "%s %s"
                            (symbol-name (car mod))
                            (symbol-name (cadr mod)))))))
          (module-string
           (completing-read
            "Describe module: "
            (cl-loop for path in (enfer-module-load-path 'all)
                     for (cat . mod) = (enfer-module-from-path path)
                     for format = (format "%s %s" cat mod)
                     if (enfer-module-p cat mod)
                     collect format
                     else if (and cat mod)
                     collect (propertize format 'face 'font-lock-comment-face))
            nil t nil nil module))
          (key (split-string module-string " ")))
     (list (intern (car  key))
           (intern (cadr key)))))
  (cl-check-type category symbol)
  (cl-check-type module symbol)
  (let ((path (enfer-module-locate-path category module)))
    (unless (file-readable-p path)
      (error "'%s %s' isn't a valid module; it doesn't exist" category module))
    (if-let* ((readme-path (enfer-module-locate-path category module "README.org")))
        (find-file readme-path)
      (if (y-or-n-p (format "The '%s %s' module has no README file. Explore its directory?"
                            category module))
          (enfer-project-browse path)
        (user-error "Aborted module lookup")))))


;;
;;; `enfer/help-packages'

(defun enfer--help-package-insert-button (label path &optional regexp)
  (declare (indent defun))
  (insert-text-button
   (string-trim label)
   'face 'link
   'follow-link t
   'action
   `(lambda (_)
      (unless (file-exists-p ,path)
        (user-error "Module doesn't exist"))
      (when (window-dedicated-p)
        (other-window 1))
      (let ((buffer (find-file ,path)))
        (when ,(stringp regexp)
          (with-current-buffer buffer
            (goto-char (point-min))
            (if (re-search-forward ,regexp nil t)
                (recenter)
              (message "Couldn't find the config block"))))))))

(defun enfer--help-packages-list (&optional refresh)
  (or (unless refresh
        (enfer-cache-get 'help-packages))
      (enfer-cache-set 'help-packages (enfer-package-list 'all))))

(defun enfer--help-package-configs (package)
  ;; TODO Add git checks, in case ~/.emacs.d isn't a git repo
  (let ((default-directory enfer-emacs-dir))
    (split-string
     (shell-command-to-string
      (format "git grep --no-break --no-heading --line-number '%s %s\\($\\| \\)'"
              "\\(^;;;###package\\|(after!\\|(def-package!\\)"
              package))
     "\n" t)))

;;;###autoload
(defun enfer/help-packages (package)
  "Like `describe-package', but for packages installed by Enfer modules.

Only shows installed packages. Includes information about where packages are
defined and configured.

If prefix arg is present, refresh the cache."
  (interactive
   (let* ((guess (or (function-called-at-point)
                     (symbol-at-point))))
     (require 'finder-inf nil t)
     (unless package--initialized
       (package-initialize t))
     (let* ((enfer--packages (enfer--help-packages-list))
            (packages (cl-delete-duplicates
                       (append (mapcar 'car package-alist)
                               (mapcar 'car package--builtins)
                               (mapcar 'car enfer--packages)
                               nil))))
       (unless (memq guess packages)
         (setq guess nil))
       (list (intern (completing-read (if guess
                                          (format "Select package to search for (default %s): "
                                                  guess)
                                        "Describe package: ")
                                      packages nil t nil nil
                                      (if guess (symbol-name guess))))))))
  (if (or (package-desc-p package)
          (and (symbolp package)
               (or (assq package package-alist)
                   (assq package package-archive-contents)
                   (assq package package--builtins))))
      (describe-package package)
    (help-setup-xref (list #'enfer/help-packages package)
                     (called-interactively-p 'interactive))
    (with-help-window (help-buffer)
      (with-current-buffer standard-output
        (prin1 package)
        (princ " is a site package.\n\n"))))
  (save-excursion
    (with-current-buffer (help-buffer)
      (let ((enfer-packages (enfer--help-packages-list))
            (inhibit-read-only t)
            (indent (make-string 13 ? )))
        (goto-char (point-min))
        (if (re-search-forward "^ *Status: " nil t)
            (progn
              (end-of-line)
              (insert "\n"))
          (re-search-forward "\n\n" nil t))

        (package--print-help-section "Source")
        (insert (or (pcase (ignore-errors (enfer-package-backend package))
                      (`elpa (concat "[M]ELPA " (enfer--package-url package)))
                      (`quelpa (format "QUELPA %s" (prin1-to-string (enfer-package-prop package :recipe))))
                      (`emacs "Built-in")
                      (_ (symbol-file package)))
                    "unknown")
                "\n")

        (when (assq package enfer-packages)
          (package--print-help-section "Modules")
          (insert "Declared by the following Enfer modules:\n")
          (dolist (m (enfer-package-prop package :modules))
            (insert indent)
            (enfer--help-package-insert-button
              (format "%s %s" (car m) (or (cdr m) ""))
              (pcase (car m)
                (:core enfer-core-dir)
                (:private enfer-private-dir)
                (category (enfer-module-path category (cdr m)))))
            (insert "\n")))

        (package--print-help-section "Configs")
        (insert "This package is configured in the following locations:")
        (dolist (location (enfer--help-package-configs package))
          (insert "\n" indent)
          (insert-text-button
           location
           'face 'link
           'follow-link t
           'action
           `(lambda (_)
              (cl-destructuring-bind (file line _match)
                  ,(split-string location ":")
                (find-file (expand-file-name file enfer-emacs-dir))
                (goto-char (point-min))
                (forward-line (1- line))
                (recenter)))))))))

(defvar enfer--package-cache nil)
(defun enfer--package-list ()
  (let* ((guess (or (function-called-at-point)
                    (symbol-at-point))))
    (require 'finder-inf nil t)
    (unless package--initialized
      (package-initialize t))
    (let ((packages (or enfer--package-cache
                        (progn
                          (message "Reading packages...")
                          (cl-delete-duplicates
                           (append (mapcar 'car package-alist)
                                   (mapcar 'car package--builtins)
                                   (mapcar 'car package-archive-contents)))))))
      (setq enfer--package-cache packages)
      (unless (memq guess packages)
        (setq guess nil))
      (intern (completing-read (if guess
                                   (format "Select package to search for (default %s): "
                                           guess)
                                 "Describe package: ")
                               packages nil t nil nil
                               (if guess (symbol-name guess)))))))

(defun enfer--package-url (package)
  (cond ((assq package package--builtins)
         (user-error "Package is built into Emacs and cannot be looked up"))
        ((when-let (location (locate-library (symbol-name package)))
           (with-temp-buffer
             (insert-file-contents (concat (file-name-sans-extension location) ".el")
                                   nil 0 4096)
             (let ((case-fold-search t))
               (when (re-search-forward " \\(?:URL\\|homepage\\|Website\\): \\(http[^\n]+\\)\n" nil t)
                 (match-string-no-properties 1))))))
        ((and (ignore-errors (eq (enfer-package-backend package) 'quelpa))
              (let* ((plist (cdr (enfer-package-prop package :recipe)))
                     (fetcher (plist-get plist :fetcher)))
                (pcase fetcher
                  (`git (plist-get plist :url))
                  (`github (format "https://github.com/%s.git" (plist-get plist :repo)))
                  (`gitlab (format "https://gitlab.com/%s.git" (plist-get plist :repo)))
                  (`bitbucket (format "https://bitbucket.com/%s" (plist-get plist :repo)))
                  (`wiki (format "https://www.emacswiki.org/emacs/download/%s"
                                 (or (car-safe (enfer-enlist (plist-get plist :files)))
                                     (format "%s.el" package))))
                  (_ (plist-get plist :url))))))
        ((and (require 'package nil t)
              (or package-archive-contents (enfer-refresh-packages-maybe))
              (pcase (package-desc-archive (cadr (assq package package-archive-contents)))
                ("org" "https://orgmode.org")
                ((or "melpa" "melpa-mirror")
                 (format "https://melpa.org/#/%s" package))
                ("elpa"
                 (format "https://elpa.gnu.org/packages/%s.html" package))
                (archive
                 (user-error "%S isn't installed through any known source (%s)"
                             package archive)))))
        ((user-error "Cannot find the homepage for %S" package))))

;;;###autoload
(defun enfer/help-package-config (package)
  "Jump to any `def-package!', `after!' or ;;;###package block for PACKAGE.

This only searches `enfer-emacs-dir' (typically ~/.emacs.d) and does not include
config blocks in your private config."
  (interactive (list (enfer--package-list)))
  (cl-destructuring-bind (file line _match)
      (split-string
       (completing-read
        "Jump to config: "
        (or (enfer--help-package-configs package)
            (user-error "This package isn't configured by you or Enfer")))
       ":")
    (find-file (expand-file-name file enfer-emacs-dir))
    (goto-char (point-min))
    (forward-line (1- line))
    (recenter)))

;;;###autoload
(defun enfer/help-package-homepage (package)
  "Open PACKAGE's repo or homepage in your browser."
  (interactive (list (enfer--package-list)))
  (browse-url (enfer--package-url package)))
