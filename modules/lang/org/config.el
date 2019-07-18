;;; lang/org/config.el -*- lexical-binding: t; -*-

;;
;;; `org-load' hooks

(defun +org|init-agenda ()
  (unless org-agenda-files
    (setq org-agenda-files (list org-directory)))
  (setq-default
   org-agenda-dim-blocked-tasks nil
   org-agenda-inhibit-startup t
   org-agenda-skip-unavailable-files t
   ;; Move the agenda to show the previous 3 days and the next 7 days for a bit
   ;; better context instead of just the current week which is a bit confusing
   ;; on, for example, a sunday
   org-agenda-span 10
   org-agenda-start-on-weekday nil
   org-agenda-start-day "-3d"))


(defun +org|init-appearance ()
  "Configures the UI for `org-mode'."
  (setq-default
   org-adapt-indentation nil
   org-cycle-include-plain-lists t
   org-eldoc-breadcrumb-separator " → "
   org-entities-user
   '(("flat"  "\\flat" nil "" "" "266D" "♭")
     ("sharp" "\\sharp" nil "" "" "266F" "♯"))
   org-fontify-done-headline t
   org-fontify-quote-and-verse-blocks t
   org-fontify-whole-heading-line t
   org-footnote-auto-label 'plain
   org-hidden-keywords nil
   org-hide-emphasis-markers nil
   org-hide-leading-stars t
   org-hide-leading-stars-before-indent-mode t
   org-image-actual-width nil
   org-indent-indentation-per-level 2
   org-indent-mode-turns-on-hiding-stars t
   org-list-description-max-indent 4
   org-pretty-entities nil
   org-pretty-entities-include-sub-superscripts t
   org-priority-faces
   '((?a . error)
     (?b . warning)
     (?c . success))
   org-refile-targets
   '((nil :maxlevel . 3)
     (org-agenda-files :maxlevel . 3))
   org-startup-folded t
   org-startup-indented t
   org-startup-with-inline-images nil
   org-tags-column 0
   org-todo-keywords
   '((sequence "TODO(t)" "PROJ(p)" "|" "FINI(d)")
     (sequence "[ ](T)" "[-](P)" "[?](M)" "|" "[X](D)")
     (sequence "SUIV(n)" "ATTE(w)" "ATTE(h)" "|" "ANNU(c)"))
   org-todo-keyword-faces
   '(("[-]" :inherit (font-lock-constant-face bold))
     ("[?]" :inherit (warning bold))
     ("PROJ" :inherit (bold default))
     ("ATTE" :inherit (warning bold))
     ("ANNU" :inherit (error bold)))
   org-use-sub-superscripts '{}

   ;; Scale up LaTeX previews a bit (default is too small)
   org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

  (advice-add #'org-eldoc-documentation-function :around #'+org*display-link-in-eldoc)

  ;; Don't do automatic indent detection in org files
  (add-to-list 'enfer-detect-indentation-excluded-modes 'org-mode nil #'eq)

  ;; Previews are usually rendered with light backgrounds, so ensure their
  ;; background (and foreground) match the current theme.
  (defun +org|update-latex-preview-background-color ()
    (setq-default
     org-format-latex-options
     (plist-put org-format-latex-options
                :background
                (face-attribute (or (cadr (assq 'default face-remapping-alist))
                                    'default)
                                :background nil t))))
  (add-hook 'enfer-load-theme-hook #'+org|update-latex-preview-background-color)

  (set-pretty-symbols! 'org-mode
                       :name "#+NAME:"
                       :src_block "#+BEGIN_SRC"
                       :src_block_end "#+END_SRC"))


(defun +org|init-babel ()
  (setq org-src-fontify-natively t      ; make code pretty
        org-src-preserve-indentation t  ; use native major-mode indentation
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window
        org-confirm-babel-evaluate nil) ; you don't need my permission

  ;; I prefer C-c C-c over C-c ' (more consistent)
  (define-key org-src-mode-map (kbd "C-c C-c") #'org-edit-src-exit)

  ;; Use major-mode native TAB indentation in SRC blocks
  (advice-add #'org-return-indent :after #'+org*fix-newline-and-indent-in-src-blocks)

  ;; `org-babel-get-header' was removed from org in 9.0. Quite a few babel
  ;; plugins use it, so until those plugins update, this polyfill will do:
  (defun org-babel-get-header (params key &optional others)
    (cl-loop with fn = (if others #'not #'identity)
             for p in params
             if (funcall fn (eq (car p) key))
             collect p))

  ;; Fixes for various babel plugins
  (setq org-babel-js-function-wrapper "console.log(require('util').inspect(function(){\n%s\n}()));"))


(defun +org|init-babel-lazy-loader ()
  "Load babel libraries lazily when babel blocks are executed."
  (defvar +org-babel-mode-alist
    '((cpp . C)
      (C++ . C)
      (D . C)
      (sh . shell)
      (bash . shell)
      (matlab . octave))
    "An alist mapping languages to babel libraries. This is necessary for babel
libraries (ob-*.el) that don't match the name of the language.

For example, with (fish . shell) will cause #+BEGIN_SRC fish to load ob-shell.el
when executed.")

  (defvar +org-babel-load-functions ()
    "A list of functions executed to load the current executing src block. They
take one argument (the language specified in the src block, as a string). Stops
at the first function to return non-nil.")

  (defun +org*src-lazy-load-library (lang)
    "Lazy load a babel package to ensure syntax highlighting."
    (or (cdr (assoc lang org-src-lang-modes))
        (fboundp (intern-soft (format "%s-mode" lang)))
        (require (intern-soft (format "ob-%s" lang)) nil t)))
  (advice-add #'org-src--get-lang-mode :before #'+org*src-lazy-load-library)

  (defun +org*babel-lazy-load-library (info)
    "Load babel libraries lazily when babel blocks are executed."
    (let* ((lang (nth 0 info))
           (lang (if (symbolp lang) lang (intern lang)))
           (lang (or (cdr (assq lang +org-babel-mode-alist))
                     lang)))
      (when (and (not (cdr (assq lang org-babel-load-languages)))
                 (or (run-hook-with-args-until-success '+org-babel-load-functions lang)
                     (require (intern (format "ob-%s" lang)) nil t)))
        (when (assq :async (nth 2 info))
          ;; ob-async has its own agenda for lazy loading packages (in the
          ;; child process), so we only need to make sure it's loaded.
          (require 'ob-async nil t))
        (add-to-list 'org-babel-load-languages (cons lang t)))
      t))
  (advice-add #'org-babel-confirm-evaluate :after-while #'+org*babel-lazy-load-library))


(defun +org|init-capture-defaults ()
  "Sets up some reasonable defaults, as well as two `org-capture' workflows that
I like:

1. The traditional way: invoking `org-capture' directly, via SPC X, or through
   the :cap ex command.
2. Through a org-capture popup frame that is invoked from outside Emacs (the
   ~/.emacs.d/bin/org-capture script). This can be invoked from qutebrowser,
   vimperator, dmenu or a global keybinding."

  (defvar +org-capture-todo-file "todo.org"
    "Default target for todo entries.

Is relative to `org-directory', unless it is absolute. Is used in Enfer's default
`org-capture-templates'.")

  (defvar +org-capture-changelog-file "changelog.org"
    "Default target for changelog entries.

Is relative to `org-directory' unless it is absolute. Is used in Enfer's default
`org-capture-templates'.")

  (defvar +org-capture-notes-file "notes.org"
    "Default target for storing notes.

Used as a fall back file for org-capture.el, for templates that do not specify a
target file.

Is relative to `org-directory', unless it is absolute. Is used in Enfer's default
`org-capture-templates'.")

  (setq org-default-notes-file
        (expand-file-name +org-capture-notes-file org-directory)
        org-capture-templates
        '(("t" "Tâche personnelle" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t :kill-buffer t)
          ("n" "Notes personnelles" entry
           (file+headline +org-capture-notes-file "Inbox")
           "* %u %?\n%i\n%a" :prepend t :kill-buffer t)

          ;; Will use {project-root}/{todo,notes,changelog}.org, unless a
          ;; {todo,notes,changelog}.org file is found in a parent directory.
          ;; Uses the basename from `+org-capture-todo-file',
          ;; `+org-capture-changelog-file' and `+org-capture-notes-file'.
          ("p" "Modèles pour projets")
          ("pt" "Tâche du projet" entry  ; {project-root}/todo.org
           (file+headline +org-capture-project-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t :kill-buffer t)
          ("pn" "Notes du projet" entry  ; {project-root}/notes.org
           (file+headline +org-capture-project-notes-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t :kill-buffer t)
          ("pc" "Changelog du projet" entry  ; {project-root}/changelog.org
           (file+headline +org-capture-project-notes-file "Unreleased")
           "* TODO %?\n%i\n%a" :prepend t :kill-buffer t)))

  (defun +org*capture-expand-variable-file (file)
    "If a variable is used for a file path in `org-capture-template', it is used
as is, and expanded relative to `default-directory'. This changes it to be
relative to `org-directory', unless it is an absolute path."
    (if (and (symbolp file) (boundp file))
        (expand-file-name (symbol-value file) org-directory)
      file))
  (advice-add #'org-capture-expand-file :filter-args #'+org*capture-expand-variable-file)

  (defun +org*prevent-save-prompts-when-refiling (&rest _)
    "Fix #462: when refiling from org-capture, Emacs prompts to kill the
underlying, modified buffer. This fixes that."
    (when (bound-and-true-p org-capture-is-refiling)
      (org-save-all-org-buffers)))
  (advice-add 'org-refile :after #'+org*prevent-save-prompts-when-refiling)

  (defun +org|show-target-in-capture-header ()
    (setq header-line-format
          (format "%s%s%s"
                  (propertize (abbreviate-file-name (buffer-file-name (buffer-base-buffer)))
                              'face 'font-lock-string-face)
                  org-eldoc-breadcrumb-separator
                  header-line-format)))
  (add-hook 'org-capture-mode-hook #'+org|show-target-in-capture-header)

  (when (featurep! :editor evil)
    (add-hook 'org-capture-mode-hook #'evil-insert-state)))


(defun +org|init-capture-frame ()
  (add-hook 'org-capture-after-finalize-hook #'+org-capture|cleanup-frame)

  (when (featurep! :ui enfer-dashboard)
    (add-hook '+enfer-dashboard-inhibit-functions #'+org-capture-frame-p)))


(defun +org|init-centralized-attachments ()
  "I believe Org's native attachment system is over-complicated and litters
files with metadata I don't want. So I wrote my own, which:

+ Places attachments in a centralized location (`org-attach-directory' in
  `org-directory').
+ Adds attach:* link abbreviation for quick links to these files from anywhere.
+ Use `+org-attach/sync' to index all attachments in `org-directory' that use
  the attach:* abbreviation and delete orphaned ones that are no longer
  referenced.
+ This compliments the +dragndrop flag which provides drag'n'drop support for
  images (with preview) and media files.

Some commands of interest:
+ `org-download-screenshot'
+ `+org-attach/file'
+ `+org-attach/url'
+ `+org-attach/sync'"
  (setq org-attach-directory (expand-file-name org-attach-directory org-directory))

  (org-link-set-parameters
   "attach"
   :follow   (lambda (link) (find-file (expand-file-name link org-attach-directory)))
   :complete (lambda (&optional _arg)
               (+org--relpath (+org-link-read-file "attach" org-attach-directory)
                              org-attach-directory))
   :face     (lambda (link)
               (if (file-exists-p (expand-file-name link org-attach-directory))
                   'org-link
                 'error)))

  (after! projectile
    (add-to-list 'projectile-globally-ignored-directories org-attach-directory))

  (after! recentf
    (add-to-list 'recentf-exclude
                 (lambda (file) (file-in-directory-p file org-attach-directory)))))


(defun +org|init-centralized-exports ()
  "TODO"
  (defvar +org-enable-centralized-exports t
    "If non-nil, files exported from files in `org-directory' will be stored in
`+org-export-directory', rather than the same directory has the input file(s).")

  (defvar +org-export-directory ".export/"
    "Where to store exported files relative to `org-directory'. Can be an absolute
path too.")

  ;; I don't have any beef with org's built-in export system, but I do wish it
  ;; would export to a central directory (by default), rather than
  ;; `default-directory'. This is because all my org files are usually in one
  ;; place, and I want to be able to refer back to old exports if needed.
  (setq +org-export-directory (expand-file-name +org-export-directory org-directory))

  (defun +org*export-output-file-name (args)
    "Return a centralized export location unless one is provided or the current
file isn't in `org-directory'."
    (when (and +org-enable-centralized-exports
               (not (nth 2 args))
               buffer-file-name
               (file-in-directory-p buffer-file-name org-directory))
      (cl-destructuring-bind (extension &optional subtreep _pubdir) args
        (let ((dir (expand-file-name +org-export-directory org-directory)))
          (unless (file-directory-p dir)
            (make-directory dir t))
          (setq args (list extension subtreep dir)))))
    args)
  (advice-add #'org-export-output-file-name :filter-args #'+org*export-output-file-name))


(defun +org|init-custom-links ()
  (defun +org--relpath (path root)
    (if (and buffer-file-name (file-in-directory-p buffer-file-name root))
        (file-relative-name path)
      path))

  (defun +org-def-link (key dir)
    (org-link-set-parameters
     key
     :complete (lambda () (+org--relpath (+org-link-read-file key dir) dir))
     :follow   (lambda (link) (find-file (expand-file-name link dir)))
     :face     (lambda (link)
                 (if (file-exists-p (expand-file-name link dir))
                     'org-link
                   'error))))


  ;; Highlight broken file links
  (org-link-set-parameters
   "file"
   :face (lambda (path)
           (if (or (file-remote-p path)
                   (file-exists-p path))
               'org-link
             'error)))


  ;; Add custom link types
  (setq org-link-abbrev-alist
        '(("github"      . "https://github.com/%s")
          ("youtube"     . "https://youtube.com/watch?v=%s")
          ("google"      . "https://google.com/search?q=")
          ("gimages"     . "https://google.com/images?q=%s")
          ("gmap"        . "https://maps.google.com/maps?q=%s")
          ("duckduckgo"  . "https://duckduckgo.com/?q=%s")
          ("wolfram"     . "https://wolframalpha.com/input/?i=%s")
          ("doom-repo"   . "https://github.com/hlissner/doom-emacs/%s")))

  (+org-def-link "org" org-directory)
  (+org-def-link "enfer" enfer-emacs-dir)
  (+org-def-link "enfer-docs" enfer-docs-dir)
  (+org-def-link "enfer-modules" enfer-modules-dir)

  ;; Allow inline image previews of http(s)? urls or data uris
  (org-link-set-parameters "http"  :image-data-fun #'+org-image-link)
  (org-link-set-parameters "https" :image-data-fun #'+org-image-link)
  (org-link-set-parameters "img"   :image-data-fun #'+org-inline-data-image)

  ;; Add support for youtube links + previews
  (def-package! org-yt))


(defun +org|init-export ()
  (when (featurep! :lang markdown)
    (add-to-list 'org-export-backends 'md))

  (def-package! ox-pandoc
    :when (and (featurep! +pandoc)
               (executable-find "pandoc"))
    :after ox
    :init
    (add-to-list 'org-export-backends 'pandoc)
    :config
    (setq org-pandoc-options
          '((standalone . t)
            (mathjax . t)
            (variable . "revealjs-url=https://cdn.jsdelivr.net/npm/reveal.js@3/")))))


(defun +org|init-habit ()
  "TODO"
  (defvar +org-habit-graph-padding 2
    "The padding added to the end of the consistency graph")

  (defvar +org-habit-min-width 30
    "Hides the consistency graph if the `org-habit-graph-column' is less than this value")

  (defvar +org-habit-graph-window-ratio 0.3
    "The ratio of the consistency graphs relative to the window width")

  (defun +org-habit|resize-graph()
    "Right align and resize the consistency graphs based on
`+org-habit-graph-window-ratio'"
    (require 'org-habit)
    (let* ((total-days (float (+ org-habit-preceding-days org-habit-following-days)))
           (preceding-days-ratio (/ org-habit-preceding-days total-days))
           (graph-width (floor (* (window-width) +org-habit-graph-window-ratio)))
           (preceding-days (floor (* graph-width preceding-days-ratio)))
           (following-days (- graph-width preceding-days))
           (graph-column (- (window-width) (+ preceding-days following-days)))
           (graph-column-adjusted (if (> graph-column +org-habit-min-width)
                                      (- graph-column +org-habit-graph-padding)
                                    nil)))
      (setq-local org-habit-preceding-days preceding-days)
      (setq-local org-habit-following-days following-days)
      (setq-local org-habit-graph-column graph-column-adjusted)))
  (add-hook 'org-agenda-mode-hook #'+org-habit|resize-graph))


(defun +org|init-hacks ()
  "Getting org to behave."
  ;; Don't open separate windows
  (setf (alist-get 'file org-link-frame-setup) #'find-file)
  ;; Open directory links in Emacs
  (add-to-list 'org-file-apps '(directory . emacs))

  (defun +org|delayed-recenter ()
    "`recenter', but after a tiny delay. Necessary to prevent certain race
conditions where a window's buffer hasn't changed at the time this hook is run."
    (run-at-time 0.1 nil #'recenter))
  (add-hook 'org-follow-link-hook #'+org|delayed-recenter)

  (defun +org*strip-properties-from-outline (orig-fn path &optional width prefix separator)
    "Remove link syntax and fix variable height text (e.g. org headings) in the
eldoc string."
    (let ((result (funcall orig-fn path width prefix separator))
          (separator (or separator "/")))
      (string-join
       (cl-loop for part
                in (split-string (substring-no-properties result) separator)
                for n from 0
                for face = (nth (% n org-n-level-faces) org-level-faces)
                collect
                (org-add-props (replace-regexp-in-string org-any-link-re "\\4" part)
                    nil 'face `(:foreground ,(face-foreground face nil t) :weight bold)))
       separator)))
  (advice-add #'org-format-outline-path :around #'+org*strip-properties-from-outline)

  (defun +org|exclude-agenda-buffers-from-workspace ()
    "Prevent from temporarily-opened agenda buffers from being associated with
the current workspace."
    (when (and org-agenda-new-buffers (bound-and-true-p persp-mode))
      (let (persp-autokill-buffer-on-remove)
        (persp-remove-buffer org-agenda-new-buffers
                             (get-current-persp)
                             nil))))
  (add-hook 'org-agenda-finalize-hook #'+org|exclude-agenda-buffers-from-workspace)

  (defun +org*exclude-agenda-buffers-from-recentf (orig-fn file)
    "Prevent temporarily opened agenda buffers from polluting recentf."
    (let ((recentf-exclude (list (lambda (_file) t))))
      (funcall orig-fn file)))
  (advice-add #'org-get-agenda-file-buffer
              :around #'+org*exclude-agenda-buffers-from-recentf))


(defun +org|init-popup-rules ()
  (set-popup-rules!
   '(("^\\*Org Links" :slot -1 :vslot -1 :size 2 :ttl 0)
     ("^\\*\\(?:Agenda Com\\|Calendar\\|Org \\(?:Export Dispatcher\\|Select\\)\\)"
      :slot -1 :vslot -1 :size #'+popup-shrink-to-fit :ttl 0)
     ("^\\*Org Agenda"    :size 0.35 :select t :ttl nil)
     ("^\\*Org Src"       :size 0.3 :quit nil :select t :autosave t :ttl nil)
     ("^CAPTURE.*\\.org$" :size 0.2 :quit nil :select t :autosave t))))


(defun +org|init-protocol-lazy-loader ()
  "Brings lazy-loaded support for org-protocol, so external programs (like
browsers) can invoke specialized behavior from Emacs. Normally you'd simply
require `org-protocol' and use it, but the package loads all of org for no
compelling reason, so..."
  (defun +org*server-visit-files (args)
    "Advise `server-visit-flist' to invoke `org-protocol' lazily."
    (cl-destructuring-bind (files proc &optional nowait) args
      (catch 'greedy
        (let ((flist (reverse files)))
          (dolist (var flist)
            (when (string-match-p ":/+" (car var))
              (require 'server)
              (require 'org-protocol)
              ;; `\' to `/' on windows
              (let ((fname (org-protocol-check-filename-for-protocol
                            (expand-file-name (car var))
                            (member var flist)
                            proc)))
                (cond ((eq fname t) ; greedy? We need the t return value.
                       (setq files nil)
                       (throw 'greedy t))
                      ((stringp fname) ; probably filename
                       (setcar var fname))
                      ((setq files (delq var files)))))))))
      (list files proc nowait)))
  (advice-add #'server-visit-files :filter-args #'+org*server-visit-files)

  ;; Disable built-in, clumsy advice
  (after! org-protocol
    (ad-disable-advice 'server-visit-files 'before 'org-protocol-detect-protocol-server)))


(defun +org|init-protocol ()
  ;; TODO org-board or better link grabbing support
  ;; TODO org-capture + org-protocol instead of bin/org-capture
  )


(defun +org|init-smartparens ()
  "TODO"
  (after! smartparens
    (defun +org-sp-point-in-checkbox-p (_id action _context)
      (and (eq action 'insert)
           (sp--looking-at-p "\\s-*]")))

    (defun +org-sp-point-at-bol-p (_id action _context)
      (and (eq action 'insert)
           (eq (char-before) ?*)
           (sp--looking-back-p "^\\**" (line-beginning-position))))

    ;; make delimiter auto-closing a little more conservative
    (sp-with-modes 'org-mode
      (sp-local-pair "*" "*" :unless '(:add sp-point-before-word-p sp-in-math-p +org-sp-point-at-bol-p))
      (sp-local-pair "_" "_" :unless '(:add sp-point-before-word-p sp-in-math-p))
      (sp-local-pair "/" "/" :unless '(:add sp-point-before-word-p sp-in-math-p +org-sp-point-in-checkbox-p))
      (sp-local-pair "~" "~" :unless '(:add sp-point-before-word-p))
      (sp-local-pair "=" "=" :unless '(:add sp-point-before-word-p sp-in-math-p)))))


;;
;;; Bootstrap

(def-package! org
  :defer-incrementally
  calendar find-func format-spec org-macs org-compat org-faces org-entities
  org-list org-pcomplete org-src org-footnote org-macro ob org org-agenda
  org-capture
  :mode ("\\.org\\'" . org-mode)
  :init
  (font-lock-add-keywords 'org-mode
                          '(("^ +\\([-*]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  :bind (("C-M-|" . indent-rigidly)
         ("C-c a" . org-agenda)
         ("C-M-|" . indent-rigidly)
         :map org-mode-map
         ("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c b" . org-iswitchb)
         ("C-c C-w" . org-refile)
         ("C-c j" . org-clock-goto)
         ("C-c C-x C-i" . org-clock-in)
         ("C-c C-x C-o" . org-clock-out)
         ("M-n" . outline-next-visible-heading)
         ("M-p" . outline-previous-visible-)
         ("M-C-n" . org-end-of-item-list)
         ("M-C-p" . org-beginning-of-item-list)
         ("C-s-f" . forward-sentence)
         ("C-s-b" . backward-sentence)
         )
  :preface
  ;; Change org defaults (should be set before org loads)
  (defvar org-directory "~/org/")
  (defvar org-attach-directory ".attach/")
  (defvar org-clock-persist-file (concat enfer-etc-dir "org-clock-save.el"))
  (defvar org-publish-timestamp-directory (concat enfer-cache-dir "org-timestamps/"))
  (defvar org-preview-latex-image-directory (concat enfer-cache-dir "org-latex/"))

  (defvar org-modules
    '(;; org-w3m
      ;; org-bbdb
      org-bibtex
      ;; org-docview
      ;; org-gnus
      ;; org-info
      ;; org-irc
      ;; org-mhe
      ;; org-rmail
      ))

  (add-hook! 'org-mode-hook
             #'(org-bullets-mode           ; "prettier" bullets
                org-indent-mode            ; margin-based indentation
                toc-org-enable             ; auto-table of contents
                auto-fill-mode             ; hard line wrapping
                ;; `show-paren-mode' causes flickering with indentation margins made by
                ;; `org-indent-mode', so we turn off show-paren-mode altogether
                enfer|disable-show-paren-mode
                ;; Shows a lot of false positives, so...
                enfer|disable-show-trailing-whitespace

                +org|enable-auto-reformat-tables
                +org|enable-auto-update-cookies
                +org|unfold-to-2nd-level-or-point))

  (add-hook! 'org-load-hook
             #'(+org|init-appearance
                +org|init-agenda
                +org|init-babel
                +org|init-babel-lazy-loader
                +org|init-capture-defaults
                +org|init-capture-frame
                +org|init-centralized-attachments
                +org|init-centralized-exports
                +org|init-custom-links
                +org|init-export
                +org|init-habit
                +org|init-hacks
                +org|init-popup-rules
                +org|init-protocol
                +org|init-protocol-lazy-loader
                +org|init-smartparens))

  ;; In case the user has eagerly loaded org from their configs
  (when (featurep 'org)
    (message "`org' was already loaded by the time lang/org loaded, this may cause issues")
    (run-hooks 'org-load-hook))

  ;;; Custom org modules
  (if (featurep! +dragndrop) (load! "contrib/dragndrop"))
  (if (featurep! +ipython)   (load! "contrib/ipython"))
  (if (featurep! +present)   (load! "contrib/present"))

  :config
  (add-hook 'org-open-at-point-functions #'enfer|set-jump)

  ;;; Packages
  (after! toc-org
          (setq toc-org-hrefify-default "gh")
          (defun +org*unfold-toc (&rest _)
            (save-excursion
              (when (re-search-forward toc-org-toc-org-regexp (point-max) t)
                (+org/open-fold))))
          (advice-add #'toc-org-insert-toc :before #'+org*unfold-toc))

  (def-package! org-pdfview
    :when (featurep! :tools pdf)
    :commands (org-pdfview-open)
    :init
    (delete '("\\.pdf\\'" . default) org-file-apps)
    ;; org links to pdf files are opened in pdf-view-mode
    (add-to-list 'org-file-apps '("\\.pdf\\'" . (lambda (_file link) (org-pdfview-open link))))
    ;; support for links to specific pages
    (add-to-list 'org-file-apps '("\\.pdf::\\([[:digit:]]+\\)\\'" . (lambda (_file link) (org-pdfview-open link)))))

  (def-package! org-crypt ; built-in
    :commands org-encrypt-entries
    :hook (org-reveal-start . org-decrypt-entry)
    :init
    (add-hook! 'org-mode-hook
               (add-hook 'before-save-hook 'org-encrypt-entries nil t))
    (add-to-list 'org-tags-exclude-from-inheritance "crypt")
    (setq org-crypt-key user-mail-address))

  (def-package! org-clock ; built-in
    :commands org-clock-save
    :hook (org-mode . org-clock-load)
    :init (setq org-clock-persist 'history)
    :config (add-hook 'kill-emacs-hook #'org-clock-save)))
