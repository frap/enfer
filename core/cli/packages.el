;; -*- no-byte-compile: t; -*-
;;; core/cli/packages.el

;;
;;; Helpers

(defmacro enfer--condition-case! (&rest body)
  `(condition-case-unless-debug e
       (progn ,@body)
     ('user-error
      (print! (bold (red "  AVIS: %s")) e))
     ('file-error
      (print! "  %s\n  %s"
              (bold (red "FICHIER ERREUR: %s" (error-message-string e)))
              "Trying again...")
      (quiet! (enfer-refresh-packages-maybe t))
      ,@body)
     ('error
      (print! (bold (red "  %s %s\n  %s"))
              "FATAL ERREUR: " e
              "Run again with the -d flag for details"))))

(defsubst enfer--ensure-autoloads-while (fn)
  (enfer-reload-enfer-autoloads)
  (when (funcall fn enfer-auto-accept)
    (enfer-reload-package-autoloads)))


;;
;;; Dispatchers

(dispatcher! (install i)
  (enfer--ensure-autoloads-while #'enfer-packages-install)
  "Installs packages that aren't installed.")

(dispatcher! (update u)
  (enfer--ensure-autoloads-while #'enfer-packages-update)
  "Updates packages.")

(dispatcher! (autoremove r)
  (enfer--ensure-autoloads-while #'enfer-packages-autoremove)
  "Removes packages that are no longer needed.")


;;
;;; Library

(defun enfer-packages-install (&optional auto-accept-p)
  "Installs missing packages.

This function will install any primary package (i.e. a package with a `package!'
declaration) or dependency thereof that hasn't already been.

Unless AUTO-ACCEPT-P is non-nil, this function will prompt for confirmation with
a list of packages that will be installed."
  (print! "Je cherche des paquets à installe...")
  (let ((packages (enfer-get-missing-packages)))
    (cond ((not packages)
           (print! (green "Pas de paquets à installer!"))
           nil)

          ((not (or auto-accept-p
                    (y-or-n-p
                     (format "%s paquets seront installés:\n\n%s\n\Procéder?"
                             (length packages)
                             (mapconcat
                              (lambda (pkg)
                                (format "+ %s (%s)"
                                        (car pkg)
                                        (cond ((enfer-package-different-recipe-p (car pkg))
                                               "new recipe")
                                              ((enfer-package-different-backend-p (car pkg))
                                               (format "%s -> %s"
                                                       (enfer-package-backend (car pkg) 'noerror)
                                                       (enfer-package-recipe-backend (car pkg) 'noerror)))
                                              ((plist-get (cdr pkg) :recipe)
                                               "quelpa")
                                              ("elpa"))))
                              (cl-sort (cl-copy-list packages) #'string-lessp
                                       :key #'car)
                              "\n")))))
           (user-error "Avortée!"))

          ((let (success)
             (enfer-refresh-packages-maybe enfer-debug-mode)
             (dolist (pkg packages)
               (print! "Installation de %s" (car pkg))
               (enfer--condition-case!
                (let ((result
                       (or (and (enfer-package-installed-p (car pkg))
                                (not (enfer-package-different-backend-p (car pkg)))
                                (not (enfer-package-different-recipe-p (car pkg)))
                                'already-installed)
                           (and (enfer-install-package (car pkg) (cdr pkg))
                                (setq success t)
                                'success)
                           'failure))
                      (pin-label
                       (and (plist-member (cdr pkg) :pin)
                            (format " [pinned: %s]" (plist-get (cdr pkg) :pin)))))
                  (print! "%s%s"
                          (pcase result
                            (`already-installed (dark (white "⚠ DÉJÀ INSTALLÉ")))
                            (`success (green "✓ FINI"))
                            (`failure (red "✕ 
ÉCHOUÉ")))
                          (or pin-label "")))))
             (print! (bold (green "Tout Fini!")))
             (when success
               (set-file-times enfer-packages-dir)
               (enfer-delete-autoloads-file enfer-package-autoload-file))
             success)))))

(defun enfer-packages-update (&optional auto-accept-p)
  "Updates packages.

Unless AUTO-ACCEPT-P is non-nil, this function will prompt for confirmation with
a list of packages that will be updated."
  (print! "Looking for outdated packages...")
  (let ((packages (cl-sort (cl-copy-list (enfer-get-outdated-packages)) #'string-lessp
                           :key #'car)))
    (cond ((not packages)
           (print! (green "Everything is up-to-date"))
           nil)

          ((not (or auto-accept-p
                    (y-or-n-p
                     (format "%s packages will be updated:\n\n%s\n\nProceed?"
                             (length packages)
                             (let ((max-len
                                    (or (car (sort (mapcar (lambda (it) (length (symbol-name (car it)))) packages)
                                                   #'>))
                                        10)))
                               (mapconcat
                                (lambda (pkg)
                                  (format (format "+ %%-%ds (%%s) %%-%ds -> %%s"
                                                  (+ max-len 2) 14)
                                          (symbol-name (car pkg))
                                          (enfer-package-backend (car pkg))
                                          (package-version-join (cadr pkg))
                                          (package-version-join (cl-caddr pkg))))
                                packages
                                "\n"))))))
           (user-error "Aborted!"))

          ((let (success)
             (dolist (pkg packages)
               (print! "Updating %s" (car pkg))
               (enfer--condition-case!
                (print!
                 (let ((result (enfer-update-package (car pkg) t)))
                   (when result (setq success t))
                   (color (if result 'green 'red)
                          (if result "✓ FINI" "✕ ÉCHOUÉ"))))))
             (print! (bold (green "Fini!")))
             (when success
               (set-file-times enfer-packages-dir)
               (enfer-delete-autoloads-file enfer-package-autoload-file))
             success)))))

(defun enfer-packages-autoremove (&optional auto-accept-p)
  "Auto-removes orphaned packages.

An orphaned package is a package that isn't a primary package (i.e. doesn't have
a `package!' declaration) or isn't depended on by another primary package.

Unless AUTO-ACCEPT-P is non-nil, this function will prompt for confirmation with
a list of packages that will be removed."
  (print! "Looking for orphaned packages...")
  (let ((packages (enfer-get-orphaned-packages)))
    (cond ((not packages)
           (print! (green "No unused packages to remove"))
           nil)

          ((not
            (or auto-accept-p
                (y-or-n-p
                 (format "%s packages will be deleted:\n\n%s\n\nProceed?"
                         (length packages)
                         (mapconcat
                          (lambda (sym)
                            (let ((old-backend (enfer-package-backend sym 'noerror))
                                  (new-backend (enfer-package-recipe-backend sym 'noerror)))
                              (format "+ %s (%s)" sym
                                      (cond ((null new-backend)
                                             "removed")
                                            ((eq old-backend new-backend)
                                             (symbol-name new-backend))
                                            ((format "%s -> %s" old-backend new-backend))))))
                          (sort (cl-copy-list packages) #'string-lessp)
                          "\n")))))
           (user-error "Aborted!"))

          ((let (success)
             (dolist (pkg packages)
               (enfer--condition-case!
                (let ((result (enfer-delete-package pkg t)))
                  (if result (setq success t))
                  (print! (color (if result 'green 'red) "%s %s")
                          (if result "✓ Enlevé" "✕ Impossible de supprimer")
                          pkg))))
             (print! (bold (green "Finition complète!")))
             (when success
               (set-file-times enfer-packages-dir)
               (enfer-delete-autoloads-file enfer-package-autoload-file))
             success)))))
