;; -*- no-byte-compile: t; -*-
;;; ui/enfer-dashboard/test/test-enfer-dashboard.el

(require 'core-projects)
(require 'projectile)
(require! :ui enfer-dashboard)

(describe "ui/enfer-dashboard"
  :var (default-directory projectile-enable-caching)
  (before-all
    (setq projectile-enable-caching nil
          enfer-fallback-buffer-name +enfer-dashboard-name))

  (before-each (projectile-mode +1))
  (after-each  (projectile-mode -1))

  (describe "get-pwd"
    :var (+enfer-dashboard--last-cwd)
    (before-each
      (setq +enfer-dashboard--last-cwd enfer-core-dir
            default-directory enfer-core-dir))
    (it "returns the current directory when policy is nil"
      (let (+enfer-dashboard-pwd-policy)
        (expect (+enfer-dashboard--get-pwd) :to-equal default-directory)))
    (it "returns a path if policy is a path"
      (let ((+enfer-dashboard-pwd-policy "~"))
        (expect (+enfer-dashboard--get-pwd) :to-equal (expand-file-name "~"))))
    (it "returns return value of policy as a function"
      (let ((+enfer-dashboard-pwd-policy (lambda (x) "x")))
        (expect (+enfer-dashboard--get-pwd) :to-equal "x")))
    (it "returns last cwd if policy is 'last"
      (let ((+enfer-dashboard-pwd-policy 'last))
        (expect (+enfer-dashboard--get-pwd) :to-equal enfer-core-dir)))
    (it "returns last project if policy is 'last-project"
      (let ((+enfer-dashboard-pwd-policy 'last-project))
        (expect (+enfer-dashboard--get-pwd) :to-equal enfer-emacs-dir))))

  (describe "dashboard-p"
    (it "changes the fallback buffer to the dashboard buffer"
      (expect (+enfer-dashboard-p (enfer-fallback-buffer))))))
