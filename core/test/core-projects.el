;; -*- no-byte-compile: t; -*-
;;; ../core/test/core-projects.el

(require 'projectile)

;;
;; `enfer-project-p'
(def-test! project-p
  :minor-mode projectile-mode
  (let ((default-directory enfer-emacs-dir))
    (should (enfer-project-p)))
  (let ((default-directory (expand-file-name "~")));
    (should-not (enfer-project-p))))

;; `enfer-project-p'
(def-test! project-root
  :minor-mode projectile-mode
  ;; Should resolve to project root
  (let ((default-directory enfer-core-dir))
    (should (equal (enfer-project-root) enfer-emacs-dir)))
  ;; Should resolve to `default-directory' if not a project
  (let ((default-directory (expand-file-name "~")))
    (should (equal (enfer-project-root) default-directory))))

;; `enfer-project-expand'
(def-test! project-expand
  :minor-mode projectile-mode
  (let ((default-directory enfer-core-dir))
    (should (equal (enfer-project-expand "init.el")
                   (expand-file-name "init.el" (enfer-project-root))))))

;; `enfer-project-has!'
(def-test! project-has!
  :minor-mode projectile-mode
  (let ((default-directory enfer-core-dir))
    ;; Resolve from project root
    (should (enfer-project-has! "init.el"))
    ;; Chained file checks
    (should (enfer-project-has! (and "init.el" "LICENSE")))
    (should (enfer-project-has! (or "init.el" "does-not-exist")))
    (should (enfer-project-has! (and "init.el" (or "LICENSE" "does-not-exist"))))
    ;; Should resolve relative paths from `default-directory'
    (should (enfer-project-has! (and "./core.el" "../init.el")))))
