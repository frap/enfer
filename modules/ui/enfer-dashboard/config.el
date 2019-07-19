;;; ui/enfer-dashboard/config.el -*- lexical-binding: t; -*-

(defvar +enfer-dashboard-name "*enfer*"
  "The name to use for the dashboard buffer.")

(defvar +enfer-dashboard-functions
  '(enfer-dashboard-widget-banner
    enfer-dashboard-widget-shortmenu
    enfer-dashboard-widget-loaded
    enfer-dashboard-widget-footer)
  "List of widget functions to run in the dashboard buffer to construct the
dashboard. These functions take no arguments and the dashboard buffer is current
while they run.")

(defvar +enfer-dashboard-banner-file "default.png"
  "The path to the image file to be used in on the dashboard. The path is
relative to `+enfer-dashboard-banner-dir'. If nil, always use the ASCII banner.")

(defvar +enfer-dashboard-banner-dir (concat (DIR!) "banners/")
  "Where to look for `+enfer-dashboard-banner-file'.")

(defvar +enfer-dashboard-banner-padding '(4 . 4)
  "Number of newlines to pad the banner with, above and below, respectively.")

(defvar +enfer-dashboard-inhibit-refresh nil
  "If non-nil, the enfer buffer won't be refreshed.")

(defvar +enfer-dashboard-inhibit-functions ()
  "A list of functions which take no arguments. If any of them return non-nil,
dashboard reloading is inhibited.")

(defvar +enfer-dashboard-pwd-policy 'last-project
  "The policy to use when setting the `default-directory' in the dashboard.

Possible values:

  'last-project  the `enfer-project-root' of the last open buffer
  'last          the `default-directory' of the last open buffer
  a FUNCTION     a function run with the `default-directory' of the last
                 open buffer, that returns a directory path
  a STRING       a fixed path
  nil            `default-directory' will never change")

(defvar +enfer-dashboard-menu-sections
  '(("Reload last session"
     :icon (all-the-icons-octicon "history" :face 'font-lock-keyword-face)
     :when (cond ((require 'persp-mode nil t)
                  (file-exists-p (expand-file-name persp-auto-save-fname persp-save-dir)))
                 ((require 'desktop nil t)
                  (file-exists-p (desktop-full-file-name))))
     :face (:inherit (font-lock-keyword-face bold))
     :action enfer/quickload-session)
    ("Open org-agenda"
     :icon (all-the-icons-octicon "calendar" :face 'font-lock-keyword-face)
     :when (fboundp 'org-agenda)
     :action org-agenda)
    ("Recently opened files"
     :icon (all-the-icons-octicon "file-text" :face 'font-lock-keyword-face)
     :action recentf-open-files)
    ("Open project"
     :icon (all-the-icons-octicon "briefcase" :face 'font-lock-keyword-face)
     :action projectile-switch-project)
    ("Jump to bookmark"
     :icon (all-the-icons-octicon "bookmark" :face 'font-lock-keyword-face)
     :action bookmark-jump)
    ("Open private configuration"
     :icon (all-the-icons-octicon "tools" :face 'font-lock-keyword-face)
     :when (file-directory-p enfer-private-dir)
     :action enfer/open-private-config)
    ("Open user manual"
     :icon (all-the-icons-octicon "book" :face 'font-lock-keyword-face)
     :when (file-exists-p (expand-file-name "index.org" enfer-docs-dir))
     :action enfer/help-search))
  "An alist of menu buttons used by `enfer-dashboard-widget-shortmenu'. Each
element is a cons cell (LABEL . PLIST). LABEL is a string to display after the
icon and before the key string.

PLIST can have the following properties:

  :icon FORM
    Uses the return value of FORM as an icon (can be literal string).
  :key STRING
    The keybind displayed next to the button.
  :when FORM
    If FORM returns nil, don't display this button.
  :face FACE
    Displays the icon and text with FACE (a face symbol).
  :action FORM
    Run FORM when the button is pushed.")

;;
(defvar +enfer-dashboard--last-cwd nil)
(defvar +enfer-dashboard--width 80)
(defvar +enfer-dashboard--old-fringe-indicator fringe-indicator-alist)
(defvar +enfer-dashboard--pwd-alist ())
(defvar +enfer-dashboard--reload-timer nil)

(defvar all-the-icons-scale-factor)
(defvar all-the-icons-default-adjust)


;;
;;; Bootstrap

(defun +enfer-dashboard|init ()
  "Initializes Enfer's dashboard."
  (unless noninteractive
    ;; Ensure the dashboard becomes Emacs' go-to buffer when there's nothing
    ;; else to show.
    (setq enfer-fallback-buffer-name +enfer-dashboard-name
          initial-buffer-choice #'enfer-fallback-buffer)
    (when (equal (buffer-name) "*scratch*")
      (set-window-buffer nil (enfer-fallback-buffer))
      (if (daemonp)
          (add-hook 'after-make-frame-functions #'+enfer-dashboard|reload-frame)
        (+enfer-dashboard-reload)))
    ;; Ensure the dashboard is up-to-date whenever it is switched to or resized.
    (add-hook 'window-configuration-change-hook #'+enfer-dashboard|resize)
    (add-hook 'window-size-change-functions #'+enfer-dashboard|resize)
    (add-hook 'enfer-switch-buffer-hook #'+enfer-dashboard|reload-maybe)
    (add-hook 'delete-frame-functions #'+enfer-dashboard|reload-frame)
    ;; `persp-mode' integration: update `default-directory' when switching perspectives
    (add-hook 'persp-created-functions #'+enfer-dashboard|record-project)
    (add-hook 'persp-activated-functions #'+enfer-dashboard|detect-project)
    (add-hook 'persp-before-switch-functions #'+enfer-dashboard|record-project)))

(add-hook 'enfer-init-ui-hook #'+enfer-dashboard|init)


;;
;;; Major mode

(define-derived-mode +enfer-dashboard-mode special-mode
  (format "ENFER v%s" enfer-version)
  "Major mode for the ENFER dashboard buffer."
  :syntax-table nil
  :abbrev-table nil
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq-local whitespace-style nil)
  (setq-local show-trailing-whitespace nil)
  (setq-local hscroll-margin 0)
  (setq-local tab-width 2)
  ;; Don't scroll to follow cursor
  (setq-local scroll-preserve-screen-position nil)
  (setq-local auto-hscroll-mode nil)
  (cl-loop for (car . _cdr) in fringe-indicator-alist
           collect (cons car nil) into alist
           finally do (setq fringe-indicator-alist alist))
  ;; Ensure point is always on a button
  (add-hook 'post-command-hook #'+enfer-dashboard|reposition-point nil t))

(bind-keys :map  +enfer-dashboard-mode-map
  ("n"   .    #'forward-button)
  ("p"    .   #'backward-button)
  ("C-n"  .   #'forward-button)
  ("C-p"  .   #'backward-button)
  ([down] .   #'forward-button)
  ([up]   .   #'backward-button)
  ([tab]  .   #'forward-button)
  ([backtab] . #'backward-button)
  )


;;
;;; Hooks

(defun +enfer-dashboard|reposition-point ()
  "Trap the point in the buttons."
  (when (region-active-p)
    (setq deactivate-mark t)
    (when (bound-and-true-p evil-local-mode)
      (evil-change-to-previous-state)))
  (or (ignore-errors
        (if (button-at (point))
            (forward-button 0)
          (backward-button 1)))
      (progn (goto-char (point-min))
             (forward-button 1))))

(defun +enfer-dashboard|reload-maybe ()
  "Reload the dashboard or its state.

If this isn't a dashboard buffer, move along, but record its `default-directory'
if the buffer is real. See `enfer-real-buffer-p' for an explanation for what
'real' means.

If this is the dashboard buffer, reload it completely."
  (cond ((+enfer-dashboard-p (current-buffer))
         (let (+enfer-dashboard-inhibit-refresh)
           (ignore-errors (+enfer-dashboard-reload))))
        ((and (not (file-remote-p default-directory))
              (enfer-real-buffer-p (current-buffer)))
         (setq +enfer-dashboard--last-cwd default-directory)
         (+enfer-dashboard-update-pwd))))

(defun +enfer-dashboard|reload-frame (_frame)
  "Reload the dashboard after a brief pause. This is necessary for new frames,
whose dimensions may not be fully initialized by the time this is run."
  (when (timerp +enfer-dashboard--reload-timer)
    (cancel-timer +enfer-dashboard--reload-timer)) ; in case this function is run rapidly
  (setq +enfer-dashboard--reload-timer (run-with-timer 0.1 nil #'+enfer-dashboard-reload t)))

(defun +enfer-dashboard|resize (&rest _)
  "Recenter the dashboard, and reset its margins and fringes."
  (let (buffer-list-update-hook
        window-configuration-change-hook
        window-size-change-functions)
    (let ((windows (get-buffer-window-list (enfer-fallback-buffer) nil t)))
      (dolist (win windows)
        (set-window-start win 0)
        (set-window-fringes win 0 0)
        (set-window-margins
         win (max 0 (/ (- (window-total-width win) +enfer-dashboard--width) 2))))
      (when windows
        (with-current-buffer (enfer-fallback-buffer)
          (save-excursion
            (with-silent-modifications
              (goto-char (point-min))
              (delete-region (line-beginning-position)
                             (save-excursion (skip-chars-forward "\n")
                                             (point)))
              (insert (make-string
                       (max 0 (- (/ (window-height (get-buffer-window)) 2)
                                 (round (/ (+ (count-lines (point-min) (point-max))
                                              (car +enfer-dashboard-banner-padding))
                                           2))))
                       ?\n)))))))))

(defun +enfer-dashboard|detect-project (&rest _)
  "Check for a `last-project-root' parameter in the perspective, and set the
dashboard's `default-directory' to it if it exists.

This and `+enfer-dashboard|record-project' provides `persp-mode' integration with
the Enfer dashboard. It ensures that the dashboard is always in the correct
project (which may be different across perspective)."
  (when (bound-and-true-p persp-mode)
    (when-let (pwd (persp-parameter 'last-project-root))
      (+enfer-dashboard-update-pwd pwd))))

(defun +enfer-dashboard|record-project (&optional persp &rest _)
  "Record the last `enfer-project-root' for the current perspective. See
`+enfer-dashboard|detect-project' for more information."
  (when (bound-and-true-p persp-mode)
    (set-persp-parameter
     'last-project-root (enfer-project-root)
     (if (persp-p persp)
         persp
       (get-current-persp)))))


;;
;;; Library

(defun +enfer-dashboard-p (buffer)
  "Returns t if BUFFER is the dashboard buffer."
  (eq buffer (get-buffer +enfer-dashboard-name)))

(defun +enfer-dashboard-update-pwd (&optional pwd)
  "Update `default-directory' in the Enfer dashboard buffer. What it is set to is
controlled by `+enfer-dashboard-pwd-policy'."
  (if pwd
      (with-current-buffer (enfer-fallback-buffer)
        (enfer-log "Changed dashboard's PWD to %s" pwd)
        (setq-local default-directory pwd))
    (let ((new-pwd (+enfer-dashboard--get-pwd)))
      (when (and new-pwd (file-directory-p new-pwd))
        (unless (string-suffix-p "/" new-pwd)
          (setq new-pwd (concat new-pwd "/")))
        (+enfer-dashboard-update-pwd new-pwd)))))

(defun +enfer-dashboard-reload (&optional force)
  "Update the ENFER scratch buffer (or create it, if it doesn't exist)."
  (when (or (and (not +enfer-dashboard-inhibit-refresh)
                 (get-buffer-window (enfer-fallback-buffer))
                 (not (window-minibuffer-p (frame-selected-window)))
                 (not (run-hook-with-args-until-success '+enfer-dashboard-inhibit-functions)))
            force)
    (with-current-buffer (enfer-fallback-buffer)
      (enfer-log "Reloading dashboard at %s" (format-time-string "%T"))
      (with-silent-modifications
        (let ((pt (point)))
          (unless (eq major-mode '+enfer-dashboard-mode)
            (+enfer-dashboard-mode))
          (erase-buffer)
          (run-hooks '+enfer-dashboard-functions)
          (goto-char pt)
          (+enfer-dashboard|reposition-point))
        (+enfer-dashboard|resize)
        (+enfer-dashboard|detect-project)
        (+enfer-dashboard-update-pwd)
        (current-buffer)))))

;; helpers
(defun +enfer-dashboard--center (len s)
  (concat (make-string (ceiling (max 0 (- len (length s))) 2) ? )
          s))

(defun +enfer-dashboard--get-pwd ()
  (let ((lastcwd +enfer-dashboard--last-cwd)
        (policy +enfer-dashboard-pwd-policy))
    (cond ((null policy)
           default-directory)
          ((stringp policy)
           (expand-file-name policy lastcwd))
          ((functionp policy)
           (funcall policy lastcwd))
          ((null lastcwd)
           default-directory)
          ((eq policy 'last-project)
           (let ((cwd default-directory))
             (or (enfer-project-root lastcwd)
                 cwd)))
          ((eq policy 'last)
           lastcwd)
          ((warn "`+enfer-dashboard-pwd-policy' has an invalid value of '%s'"
                 policy)))))


;;
;;; Widgets

(defun enfer-dashboard-widget-banner ()
  (let ((point (point)))
    (mapc (lambda (line)
            (insert (propertize (+enfer-dashboard--center +enfer-dashboard--width line)
                                'face 'font-lock-comment-face) " ")
            (insert "\n"))
          '("=================     ===============     ===============   ========  ========"
            "\\\\ . . . . . . .\\\\   //. . . . . . .\\\\   //. . . . . . .\\\\  \\\\. . .\\\\// . . //"
            "||. . ._____. . .|| ||. . ._____. . .|| ||. . ._____. . .|| || . . .\\/ . . .||"
            "|| . .||   ||. . || || . .||   ||. . || || . .||   ||. . || ||. . . . . . . ||"
            "||. . ||   || . .|| ||. . ||   || . .|| ||. . ||   || . .|| || . | . . . . .||"
            "|| . .||   ||. _-|| ||-_ .||   ||. . || || . .||   ||. _-|| ||-_.|\\ . . . . ||"
            "||. . ||   ||-'  || ||  `-||   || . .|| ||. . ||   ||-'  || ||  `|\\_ . .|. .||"
            "|| . _||   ||    || ||    ||   ||_ . || || . _||   ||    || ||   |\\ `-_/| . ||"
            "||_-' ||  .|/    || ||    \\|.  || `-_|| ||_-' ||  .|/    || ||   | \\  / |-_.||"
            "||    ||_-'      || ||      `-_||    || ||    ||_-'      || ||   | \\  / |  `||"
            "||    `'         || ||         `'    || ||    `'         || ||   | \\  / |   ||"
            "||            .===' `===.         .==='.`===.         .===' /==. |  \\/  |   ||"
            "||         .=='   \\_|-_ `===. .==='   _|_   `===. .===' _-|/   `==  \\/  |   ||"
            "||      .=='    _-'    `-_  `='    _-'   `-_    `='  _-'   `-_  /|  \\/  |   ||"
            "||   .=='    _-'          '-__\\._-'         '-_./__-'         `' |. /|  |   ||"
            "||.=='    _-'                                                     `' |  /==.||"
            "=='    _-'                         E M A C S                          \\/   `=="
            "\\   _-'                                                                `-_   /"
            " `''                                                                      ``'"))
    (when (and (stringp +enfer-dashboard-banner-file)
               (display-graphic-p)
               (file-exists-p! +enfer-dashboard-banner-file +enfer-dashboard-banner-dir))
      (let* ((image (create-image (expand-file-name +enfer-dashboard-banner-file
                                                    +enfer-dashboard-banner-dir)
                                  'png nil))
             (size (image-size image nil))
             (margin (+ 1 (/ (- +enfer-dashboard--width (car size)) 2))))
        (add-text-properties
         point (point) `(display ,image rear-nonsticky (display)))
        (when (> margin 0)
          (save-excursion
            (goto-char point)
            (insert (make-string (truncate margin) ? )))))
      (insert (make-string (or (cdr +enfer-dashboard-banner-padding) 0) ?\n)))))

(defun enfer-dashboard-widget-loaded ()
  (insert
   "\n\n"
   (propertize
    (+enfer-dashboard--center
     +enfer-dashboard--width
     (enfer|display-benchmark 'return))
    'face 'font-lock-comment-face)
   "\n"))

(defun enfer-dashboard-widget-shortmenu ()
  (let ((all-the-icons-scale-factor 1.45)
        (all-the-icons-default-adjust -0.02))
    (insert "\n")
    (dolist (section +enfer-dashboard-menu-sections)
      (cl-destructuring-bind (label &key icon action when face) section
        (when (and (fboundp action)
                   (or (null when)
                       (eval when t)))
          (insert
           (+enfer-dashboard--center
            (- +enfer-dashboard--width 1)
            (let ((icon (if (stringp icon) icon (eval icon t))))
              (format (format "%s%%s%%-10s" (if icon "%3s\t" "%3s"))
                      (or icon "")
                      (with-temp-buffer
                        (insert-text-button
                         label
                         'action
                         `(lambda (_)
                            (call-interactively (or (command-remapping #',action)
                                                    #',action)))
                         'face (or face 'font-lock-keyword-face)
                         'follow-link t
                         'help-echo
                         (format "%s (%s)" label
                                 (propertize (symbol-name action) 'face 'font-lock-constant-face)))
                        (format "%-37s" (buffer-string)))
                      ;; Lookup command keys dynamically
                      (or (when-let (key (where-is-internal action nil t))
                            (with-temp-buffer
                              (save-excursion (insert (key-description key)))
                              (while (re-search-forward "<\\([^>]+\\)>" nil t)
                                (let ((str (match-string 1)))
                                  (replace-match
                                   (upcase (if (< (length str) 3)
                                               str
                                             (substring str 0 3))))))
                              (propertize (buffer-string) 'face 'font-lock-constant-face)))
                          ""))))
           (if (display-graphic-p)
               "\n\n"
             "\n")))))))

(defun enfer-dashboard-widget-footer ()
  (insert
   "\n"
   (+enfer-dashboard--center
    (- +enfer-dashboard--width 2)
    (with-temp-buffer
      (insert-text-button (or (all-the-icons-octicon "octoface" :face 'all-the-icons-green :height 1.3 :v-adjust -0.15)
                              (propertize "github" 'face 'font-lock-keyword-face))
                          'action (lambda (_) (browse-url "https://github.com/hlissner/doom-emacs"))
                          'follow-link t
                          'help-echo "Open Enfer Emacs github page")
      (buffer-string)))
   "\n"))
