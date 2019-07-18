;;; ui/modeline/config.el -*- lexical-binding: t; -*-

(def-package! enfer-modeline
  :hook (after-init . enfer-modeline-mode)
  :init
  (unless after-init-time
    ;; prevent flash of unstyled modeline at startup
    (setq-default mode-line-format nil))
  ;; We display project info in the modeline ourselves
  (setq projectile-dynamic-mode-line nil)
  ;; Set these early so they don't trigger variable watchers
  (setq enfer-modeline-bar-width 3
        enfer-modeline-github nil
        enfer-modeline-mu4e nil
        enfer-modeline-persp-name nil
        enfer-modeline-minor-modes nil
        enfer-modeline-major-mode-icon nil
        enfer-modeline-buffer-file-name-style 'relative-from-project)

  ;; Fix modeline icons in daemon-spawned graphical frames. We have our own
  ;; mechanism for disabling all-the-icons, so we don't need enfer-modeline to do
  ;; it for us. However, this may cause unwanted padding in the modeline in
  ;; daemon-spawned terminal frames. If it bothers you, you may prefer
  ;; `enfer-modeline-icon' set to `nil'.
  (when (daemonp)
    (setq enfer-modeline-icon t))
  :config
  ;; Fix an issue where these two variables aren't defined in TTY Emacs on MacOS
  (defvar mouse-wheel-down-event nil)
  (defvar mouse-wheel-up-event nil)

  (add-hook 'enfer-modeline-mode-hook #'size-indication-mode) ; filesize in modeline
  (add-hook 'enfer-modeline-mode-hook #'column-number-mode)   ; cursor column in modeline

  (add-hook 'enfer-change-font-size-hook #'+modeline|resize-for-font)
  (add-hook 'enfer-load-theme-hook #'enfer-modeline-refresh-bars)

  (add-hook '+enfer-dashboard-mode-hook #'enfer-modeline-set-project-modeline)

  (defun +modeline|hide-in-non-status-buffer ()
    "Show minimal modeline in magit-status buffer, no modeline elsewhere."
    (if (eq major-mode 'magit-status-mode)
        (enfer-modeline-set-project-modeline)
      (hide-mode-line-mode)))
  (add-hook 'magit-mode-hook #'+modeline|hide-in-non-status-buffer)

  ;; Remove unused segments & extra padding
  (enfer-modeline-def-modeline 'main
                               '(bar window-number matches buffer-info remote-host buffer-position selection-info)
                               '(objed-state misc-info persp-name irc mu4e github debug input-method buffer-encoding lsp major-mode process vcs checker))

  (enfer-modeline-def-modeline 'special
                               '(bar window-number matches buffer-info-simple buffer-position selection-info)
                               '(objed-state misc-info persp-name debug input-method irc-buffers buffer-encoding lsp major-mode process checker))

  (enfer-modeline-def-modeline 'project
                               '(bar window-number buffer-default-directory)
                               '(misc-info mu4e github debug fancy-battery " " major-mode process))

  ;; Some functions modify the buffer, causing the modeline to show a false
  ;; modified state, so we try to force them to behave.
  (defun +modeline*inhibit-modification-hooks (orig-fn &rest args)
    (with-silent-modifications (apply orig-fn args)))
  (advice-add #'ws-butler-after-save :around #'+modeline*inhibit-modification-hooks))


;;
;; Extensions

(def-package! anzu
  :after-call isearch-mode)
