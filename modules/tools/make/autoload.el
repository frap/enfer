;;; tools/make/autoload.el -*- lexical-binding: t; -*-

(require 'makefile-executor)

;;;###autoload
(defun +make/run ()
  "Run a make task in the current project. If multiple makefiles are available,
you'll be prompted to select one."
  (interactive)
  (if (enfer-project-p)
      (makefile-executor-execute-project-target)
    (let ((makefile (cl-loop with buffer-file = (or buffer-file-name default-directory)
                             for file in (list "Makefile" "makefile")
                             if (locate-dominating-file buffer-file file)
                             return file)))
      (unless makefile
        (user-error "Impossible de trouver un fichier makefile dans le projet en cours"))
      (let ((default-directory (file-name-directory makefile)))
        (makefile-executor-execute-target makefile)))))

;;;###autoload
(defun +make/run-last ()
  "TODO"
  (interactive)
  (makefile-executor-execute-last nil))