;;; core/cli/test.el -*- lexical-binding: t; -*-

(dispatcher! test (enfer-run-tests args)
  "Run Enfer unit tests.")


;;
;; Library

(defun enfer-run-tests (&optional modules)
  "Run all loaded tests, specified by MODULES (a list of module cons cells) or
command line args following a double dash (each arg should be in the
'module/submodule' format).

If neither is available, run all tests in all enabled modules."
  ;; Core libraries aren't fully loaded in a noninteractive session, so we
  ;; reload it with `noninteractive' set to nil to force them to.
  (let* ((noninteractive t)
         (enfer-modules (enfer-modules)))
    (quiet! (enfer-reload-autoloads))
    (let ((target-paths
           ;; Convert targets into a list of string paths, pointing to the root
           ;; directory of modules
           (cond ((stringp (car modules)) ; command line
                  (save-match-data
                    (cl-loop for arg in modules
                             if (string= arg ":core") collect enfer-core-dir
                             else if (string-match-p "/" arg)
                             nconc (mapcar (apply-partially #'expand-file-name arg)
                                           enfer-modules-dirs)
                             else
                             nconc (cl-loop for dir in enfer-modules-dirs
                                            for path = (expand-file-name arg dir)
                                            if (file-directory-p path)
                                            nconc (enfer-files-in path :type 'dirs :depth 1 :full t :sort nil))
                             finally do (setq argv nil))))

                 (modules ; cons-cells given to MODULES
                  (cl-loop for (module . submodule) in modules
                           if (enfer-module-locate-path module submodule)
                           collect it))

                 ((append (list enfer-core-dir)
                          (enfer-module-load-path))))))
      ;; Load all the unit test files...
      (require 'buttercup)
      (mapc (lambda (file) (load file :noerror (not enfer-debug-mode)))
            (enfer-files-in (mapcar (apply-partially #'expand-file-name "test/")
                                   target-paths)
                           :match "\\.el$" :full t))
      ;; ... then run them
      (when enfer-debug-mode
        (setq buttercup-stack-frame-style 'pretty))
      (let ((split-width-threshold 0)
            (split-height-threshold 0)
            (window-min-width 0)
            (window-min-height 0))
        (buttercup-run)))))


;;
;; Test library

(defmacro insert! (&rest text)
  "Insert TEXT in buffer, then move cursor to last {0} marker."
  `(progn
     (insert ,@text)
     (when (search-backward "{0}" nil t)
       (replace-match "" t t))))
