;; -*- no-byte-compile: t; -*-
;;; core/test/autoload-package.el

(defun -pkg (name version &optional reqs)
  (package-desc-create :name name :version version :reqs reqs))

(defmacro with-packages!! (packages package-descs &rest body)
`(let* ((enfer-packages-dir ,(expand-file-name "packages/" (file-name-directory load-file-name)))
        (package-user-dir ,(expand-file-name "elpa" enfer-packages-dir))
        (quelpa-dir ,(expand-file-name "quelpa" enfer-packages-dir)))
   ;; (make-directory enfer-packages-dir t)
   (let ((enfer-packages ,packages)
         (package-alist ,package-descs)
         enfer-core-packages)
     (cl-letf (((symbol-function 'enfer-initialize-packages) (lambda (&rest _))))
       ,@body))
   ;; (delete-directory enfer-packages-dir t)
   ))


;;
;; Tests
;;

(def-test! backend-detection
  (let ((package-alist `((enfer-dummy ,(-pkg 'enfer-dummy '(20160405 1234)))))
        (quelpa-cache '((enfer-quelpa-dummy :fetcher github :repo "hlissner/does-not-exist")))
        (quelpa-initialized-p t))
    (should (eq (enfer-package-backend 'enfer-dummy) 'elpa))
    (should (eq (enfer-package-backend 'enfer-quelpa-dummy) 'quelpa))
    (should (eq (enfer-package-backend 'org) 'emacs))))

(def-test! elpa-outdated-detection
  (let* ((enfer--last-refresh (current-time))
         (package-alist
          `((enfer-dummy ,(-pkg 'enfer-dummy '(20160405 1234)))))
         (package-archive-contents
          `((enfer-dummy ,(-pkg 'enfer-dummy '(20170405 1234))))))
    (cl-letf (((symbol-function 'package-refresh-contents) (lambda (&rest _))))
      (should (equal (enfer-package-outdated-p 'enfer-dummy)
                     '(enfer-dummy (20160405 1234) (20170405 1234)))))))

;; TODO quelpa-outdated-detection

(def-test! get-packages
  (let ((quelpa-initialized-p t))
    (with-packages!!
     '((enfer-dummy))
     '((enfer-dummy          nil)
       (enfer-dummy-unwanted nil)
       (enfer-dummy-dep      nil))
     (should (equal (enfer-get-packages) '((enfer-dummy)))))))

(def-test! orphaned-packages
  "Test `enfer-get-orphaned-packages', which gets a list of packages that are
no longer enabled or depended on."
  (with-packages!!
   '((enfer-dummy))
   `((enfer-dummy          ,(-pkg 'enfer-dummy '(20160405 1234) '((enfer-dummy-dep (1 0)))))
     (enfer-dummy-unwanted ,(-pkg 'enfer-dummy-unwanted '(20160601 1234)))
     (enfer-dummy-dep      ,(-pkg 'enfer-dummy-dep '(20160301 1234))))
   (should (equal (enfer-get-orphaned-packages) '(enfer-dummy-unwanted)))))

(def-test! missing-packages
  "Test `enfer-get-missing-packages, which gets a list of enabled packages that
aren't installed."
  (with-packages!!
   '((enfer-dummy) (enfer-dummy-installed))
   `((enfer-dummy-installed ,(-pkg 'enfer-dummy-installed '(20160405 1234))))
   (should (equal (enfer-get-missing-packages) '((enfer-dummy))))))
