;;; core-editor.el -*- lexical-binding: t; -*-

(defvar enfer-large-file-size 2
  "Size (in MB) above which the user will be prompted to open the file literally
to avoid performance issues. Opening literally means that no major or minor
modes are active and the buffer is read-only.")

(defvar enfer-large-file-modes-list
  '(fundamental-mode special-mode archive-mode tar-mode jka-compr
    git-commit-mode image-mode doc-view-mode doc-view-mode-maybe
    ebrowse-tree-mode pdf-view-mode tags-table-mode)
  "Major modes that `enfer|check-large-file' will ignore.")

(defvar-local enfer-inhibit-indent-detection nil
  "A buffer-local flag that indicates whether `dtrt-indent' should try to detect
indentation settings or not. This should be set by editorconfig if it
successfully sets indent_style/indent_size.")

(defvar enfer-detect-indentation-excluded-modes '(fundamental-mode)
  "A list of major modes in which indentation should be automatically
detected.")

(setq-default
 large-file-warning-threshold 15000000
 vc-follow-symlinks t
 ;; Save clipboard contents into kill-ring before replacing them
 save-interprogram-paste-before-kill t
 ;; Bookmarks
 bookmark-default-file (concat enfer-etc-dir "bookmarks")
 bookmark-save-flag t
 ;; Formatting
 delete-trailing-lines nil
 fill-column 80
 sentence-end-double-space nil
 word-wrap t
 ;; Scrolling
 hscroll-margin 2
 hscroll-step 1
 scroll-conservatively 1001
 scroll-margin 0
 scroll-preserve-screen-position t
 mouse-wheel-scroll-amount '(5 ((shift) . 2))
 mouse-wheel-progressive-speed nil ; don't accelerate scrolling
 ;; Whitespace (see `editorconfig')
 indent-tabs-mode nil
 require-final-newline t
 tab-always-indent t
 tab-width 4
 tabify-regexp "^\t* [ \t]+" ; for :retab
 ;; Wrapping
 truncate-lines t
 truncate-partial-width-windows 50)

;; Remove hscroll-margin in shells, otherwise it causes jumpiness
(setq-hook! '(eshell-mode-hook term-mode-hook) hscroll-margin 0)

(defun enfer*optimize-literal-mode-for-large-files (buffer)
  (with-current-buffer buffer
    (when find-file-literally
      (setq buffer-read-only t)
      (buffer-disable-undo))
    buffer))
(advice-add #'find-file-noselect-1 :filter-return #'enfer*optimize-literal-mode-for-large-files)


;;
;;; Extra file extensions to support

(push '("/LICENSE\\'" . text-mode) auto-mode-alist)


;;
;;; Built-in plugins

(def-package! autorevert
  ;; revert buffers when their files/state have changed
  :hook (focus-in . enfer|auto-revert-buffers)
  :hook (after-save . enfer|auto-revert-buffers)
  :hook (enfer-switch-buffer . enfer|auto-revert-buffer)
  :hook (enfer-switch-window . enfer|auto-revert-buffer)
  :config
  (setq auto-revert-verbose t ; let us know when it happens
        auto-revert-use-notify nil
        auto-revert-stop-on-user-input nil)

  ;; Instead of using `auto-revert-mode' or `global-auto-revert-mode', we employ
  ;; lazy auto reverting on `focus-in-hook' and `enfer-switch-buffer-hook'.
  ;;
  ;; This is because autorevert abuses the heck out of inotify handles which can
  ;; grind Emacs to a halt if you do expensive IO (outside of Emacs) on the
  ;; files you have open (like compression). We only really need revert changes
  ;; when we switch to a buffer or when we focus the Emacs frame.
  (defun enfer|auto-revert-buffers ()
    "Auto revert's stale buffers (that are visible)."
    (unless auto-revert-mode
      (dolist (buf (enfer-visible-buffers))
        (with-current-buffer buf
          (auto-revert-handler)))))

  (defun enfer|auto-revert-buffer ()
    "Auto revert current buffer, if necessary."
    (unless auto-revert-mode
      (auto-revert-handler))))

(def-package! recentf
  ;; Keep track of recently opened files
  :defer-incrementally (easymenu tree-widget timer)
  :after-call after-find-file
  :commands recentf-open-files
  :config
  (setq recentf-save-file (concat enfer-cache-dir "recentf")
        recentf-auto-cleanup 'never
        recentf-max-menu-items 0
        recentf-max-saved-items 200
        recentf-exclude
        (list "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$" "^/tmp/" "^/ssh:"
              "\\.?ido\\.last$" "\\.revive$" "/TAGS$" "^/var/folders/.+$"
              ;; ignore private ENFER temp files
              (lambda (path)
                (ignore-errors (file-in-directory-p path enfer-local-dir)))))

  (defun enfer--recent-file-truename (file)
    (if (or (file-remote-p file nil t)
            (not (file-remote-p file)))
        (file-truename file)
      file))
  (setq recentf-filename-handlers '(enfer--recent-file-truename abbreviate-file-name))

  (defun enfer|recentf-touch-buffer ()
    "Bump file in recent file list when it is switched or written to."
    (when buffer-file-name
      (recentf-add-file buffer-file-name))
    ;; Return nil for `write-file-functions'
    nil)
  (add-hook 'enfer-switch-window-hook #'enfer|recentf-touch-buffer)
  (add-hook 'write-file-functions #'enfer|recentf-touch-buffer)

  (defun enfer|recentf-add-dired-directory ()
    "Add dired directory to recentf file list."
    (recentf-add-file default-directory))
  (add-hook 'dired-mode-hook #'enfer|recentf-add-dired-directory)

  (unless noninteractive
    (add-hook 'kill-emacs-hook #'recentf-cleanup)
    (quiet! (recentf-mode +1))))

(def-package! savehist
  ;; persist variables across sessions
  :defer-incrementally (custom)
  :after-call post-command-hook
  :config
  (setq savehist-file (concat enfer-cache-dir "savehist")
        savehist-save-minibuffer-history t
        savehist-autosave-interval nil ; save on kill only
        savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (savehist-mode +1)

  (defun enfer|unpropertize-kill-ring ()
    "Remove text properties from `kill-ring' in the interest of shrinking the
savehist file."
    (setq kill-ring (cl-loop for item in kill-ring
                             if (stringp item)
                             collect (substring-no-properties item)
                             else if item collect it)))
  (add-hook 'kill-emacs-hook #'enfer|unpropertize-kill-ring))

(def-package! saveplace
  ;; persistent point location in buffers
  :after-call (after-find-file dired-initial-position-hook)
  :config
  (setq save-place-file (concat enfer-cache-dir "saveplace")
        save-place-forget-unreadable-files t
        save-place-limit 200)
  (defun enfer*recenter-on-load-saveplace (&rest _)
    "Recenter on cursor when loading a saved place."
    (if buffer-file-name (ignore-errors (recenter))))
  (advice-add #'save-place-find-file-hook
              :after-while #'enfer*recenter-on-load-saveplace)
  (save-place-mode +1))

(def-package! server
  :when (display-graphic-p)
  :after-call (pre-command-hook after-find-file)
  :init
  (when-let (name (getenv "EMACS_SERVER_NAME"))
    (setq server-name name))
  :config
  (unless (server-running-p)
    (server-start)))


;;
;;; Packages

(def-package! better-jumper
  :after-call (pre-command-hook)
  :init
  (global-set-key [remap evil-jump-forward]  #'better-jumper-jump-forward)
  (global-set-key [remap evil-jump-backward] #'better-jumper-jump-backward)
  :config
  (better-jumper-mode +1)
  (add-hook 'better-jumper-post-jump-hook #'recenter)

  (defun enfer*set-jump (orig-fn &rest args)
    "Set a jump point and ensure ORIG-FN doesn't set any new jump points."
    (better-jumper-set-jump (if (markerp (car args)) (car args)))
    (let ((evil--jumps-jumping t)
          (better-jumper--jumping t))
      (apply orig-fn args)))

  (defun enfer*set-jump-maybe (orig-fn &rest args)
    "Set a jump point if ORIG-FN returns non-nil."
    (let ((origin (point-marker))
          (result
           (let* ((evil--jumps-jumping t)
                  (better-jumper--jumping t))
             (apply orig-fn args))))
      (unless result
        (with-current-buffer (marker-buffer origin)
          (better-jumper-set-jump
           (if (markerp (car args))
               (car args)
             origin))))
      result))

  (defun enfer|set-jump ()
    "Run `better-jumper-set-jump' but return nil, for short-circuiting hooks."
    (better-jumper-set-jump)
    nil))


(def-package! command-log-mode
  :commands global-command-log-mode
  :config
  (setq command-log-mode-auto-show t
        command-log-mode-open-log-turns-on-mode nil
        command-log-mode-is-global t
        command-log-mode-window-size 50))


(def-package! dtrt-indent
  ;; Automatic detection of indent settings
  :unless noninteractive
  :defer t
  :init
  (defun enfer|detect-indentation ()
    (unless (or (not after-init-time)
                enfer-inhibit-indent-detection
                (member (substring (buffer-name) 0 1) '(" " "*"))
                (memq major-mode enfer-detect-indentation-excluded-modes))
      ;; Don't display messages in the echo area, but still log them
      (let ((inhibit-message (not enfer-debug-mode)))
        (dtrt-indent-mode +1))))
  (add-hook! '(change-major-mode-after-body-hook read-only-mode-hook)
    #'enfer|detect-indentation)
  :config
  (setq dtrt-indent-run-after-smie t)

  ;; always keep tab-width up-to-date
  (push '(t tab-width) dtrt-indent-hook-generic-mapping-list)

  (defvar dtrt-indent-run-after-smie)
  (defun enfer*fix-broken-smie-modes (orig-fn arg)
    "Some smie modes throw errors when trying to guess their indentation, like
`nim-mode'. This prevents them from leaving Emacs in a broken state."
    (let ((dtrt-indent-run-after-smie dtrt-indent-run-after-smie))
      (cl-letf* ((old-smie-config-guess (symbol-function 'smie-config-guess))
                 ((symbol-function 'smie-config-guess)
                  (lambda ()
                    (condition-case e (funcall old-smie-config-guess)
                      (error (setq dtrt-indent-run-after-smie t)
                             (message "[WARNING] Indent detection: %s"
                                      (error-message-string e))
                             (message "")))))) ; warn silently
        (funcall orig-fn arg))))
  (advice-add #'dtrt-indent-mode :around #'enfer*fix-broken-smie-modes))


(def-package! helpful
  ;; a better *help* buffer
  :commands helpful--read-symbol
  :init
 ;; (bind-key
 ;;   [remap describe-function] #'helpful-callable
 ;;   [remap describe-command]  #'helpful-command
 ;;   [remap describe-variable] #'helpful-variable
 ;;   [remap describe-key]      #'helpful-key
 ;;   [remap describe-symbol]   #'enfer/describe-symbol)

  (after! apropos
    ;; patch apropos buttons to call helpful instead of help
    (dolist (fun-bt '(apropos-function apropos-macro apropos-command))
      (button-type-put
       fun-bt 'action
       (lambda (button)
         (helpful-callable (button-get button 'apropos-symbol)))))
    (dolist (var-bt '(apropos-variable apropos-user-option))
      (button-type-put
       var-bt 'action
       (lambda (button)
         (helpful-variable (button-get button 'apropos-symbol)))))))


;;;###package imenu
(add-hook 'imenu-after-jump-hook #'recenter)

(defun zz/sp-enclose-next-sexp (num) (interactive "p") (insert-parentheses (or num 1)))
(def-package! smartparens
  ;; Auto-close delimiters and blocks as you type. It's more powerful than that,
  ;; but that is all Enfer uses it for.
  :after-call (enfer-switch-buffer-hook after-find-file)
  :commands (sp-pair sp-local-pair sp-with-modes sp-point-in-comment sp-point-in-string)
   :bind (("M-n" . sp-next-sexp)
         ("M-p" . sp-previous-sexp)
         ("M-f" . sp-forward-sexp)
         ("M-b" . sp-backward-sexp))
  :config
  (require 'smartparens-config)
  (sp-pair "=" "=" :actions '(wrap))
  (sp-pair "+" "+" :actions '(wrap))
  (sp-pair "<" ">" :actions '(wrap))
  (sp-pair "$" "$" :actions '(wrap))
  ;; Close a backtick with another backtick in clojure-mode
  (sp-local-pair 'clojure-mode "`" "`" :when '(sp-in-string-p))
  (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p))
  :hook
  ;; (after-init . smartparens-global-mode)
  ((clojure-mode
    emacs-lisp-mode
    lisp-mode) . smartparens-strict-mode)
  (smartparens-mode  . sp-use-paredit-bindings)
  (smartparens-mode  . (lambda () (local-set-key (kbd "M-(") 'zz/sp-enclose-next-sexp)))

  )


(def-package! undo-tree
  ;; Branching & persistent undo
  :after-call (enfer-switch-buffer-hook after-find-file)
  :config
  (setq undo-tree-auto-save-history nil ; disable because unstable
        ;; undo-in-region is known to cause undo history corruption, which can
        ;; be very destructive! Disabling it deters the error, but does not fix
        ;; it entirely!
        undo-tree-enable-undo-in-region nil
        undo-tree-history-directory-alist
        `(("." . ,(concat enfer-cache-dir "undo-tree-hist/"))))

  (when (executable-find "zstd")
    (defun enfer*undo-tree-make-history-save-file-name (file)
      (concat file ".zst"))
    (advice-add #'undo-tree-make-history-save-file-name :filter-return
                #'enfer*undo-tree-make-history-save-file-name))

  (defun enfer*strip-text-properties-from-undo-history (&rest _)
    (dolist (item buffer-undo-list)
      (and (consp item)
           (stringp (car item))
           (setcar item (substring-no-properties (car item))))))
  (advice-add #'undo-list-transfer-to-tree :before #'enfer*strip-text-properties-from-undo-history)

  (global-undo-tree-mode +1))


(def-package! ws-butler
  ;; a less intrusive `delete-trailing-whitespaces' on save
  :after-call (after-find-file)
  :config
  (setq ws-butler-global-exempt-modes
        (append ws-butler-global-exempt-modes
                '(special-mode comint-mode term-mode eshell-mode)))
  (ws-butler-global-mode))

(provide 'core-editor)
;;; core-editor.el ends here
