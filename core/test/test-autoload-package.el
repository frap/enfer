;; -*- no-byte-compile: t; -*-
;;; core/test/test-autoload-package.el

(describe "core/autoload/packages"
  :var (package-alist
        package-archive-contents
        package-selected-packages
        enfer-packages
        quelpa-cache
        quelpa-initialized-p
        enfer-packages-dir
        enfer-core-packages
        package-user-dir
        quelpa-dir
        pkg)

  (before-all
    (fset 'pkg
          (lambda (name version &optional reqs)
            (package-desc-create
             :name name :version version :reqs reqs
             :dir (expand-file-name (format "%s/" name) package-user-dir))))
    (require 'package)
    (require 'quelpa)
    (setq enfer-packages-dir (expand-file-name "packages/" (file-name-directory load-file-name))
          package-user-dir (expand-file-name "elpa" enfer-packages-dir)
          quelpa-dir (expand-file-name "quelpa" enfer-packages-dir)
          quelpa-initialized-p t
          enfer-core-packages nil)
    (spy-on #'package--user-installed-p :and-call-fake (lambda (_p) t))
    (spy-on #'enfer-initialize-packages :and-call-fake (lambda (&optional _)))
    (spy-on #'package-refresh-contents :and-call-fake (lambda (&optional _)))
    (spy-on #'quelpa-checkout :and-call-fake
            (lambda (rcp _dir)
              (when (eq (car rcp) 'enfer-quelpa-dummy)
                "20170405.1234"))))

  (after-all
    (unload-feature 'package t)
    (unload-feature 'quelpa t))

  (before-each
    (setq package-alist
          `((enfer-dummy ,(pkg 'enfer-dummy '(20160405 1234)))
            (enfer-uptodate-dummy ,(pkg 'enfer-uptodate-dummy '(20160605 1234)))
            (enfer-unwanted-dummy ,(pkg 'enfer-unwanted-dummy '(20160605 1234)))
            (enfer-quelpa-dummy ,(pkg 'enfer-quelpa-dummy '(20160405 1234)))
            (enfer-noquelpa-dummy ,(pkg 'enfer-noquelpa-dummy '(20160405 1234))))
          package-archive-contents
          `((enfer-dummy ,(pkg 'enfer-dummy '(20170405 1234)))
            (enfer-uptodate-dummy ,(pkg 'enfer-uptodate-dummy '(20160605 1234))))
          enfer-packages
          '((enfer-dummy)
            (enfer-uptodate-dummy)
            (enfer-missing-dummy)
            (enfer-noquelpa-dummy)
            (enfer-disabled-dummy :disable t)
            (enfer-private-dummy :modules ((:private)))
            (enfer-disabled-private-dummy :modules ((:private)) :disable t)
            (enfer-quelpa-dummy :recipe (enfer-quelpa-dummy :fetcher github :repo "hlissner/does-not-exist")))
          quelpa-cache
          '((enfer-quelpa-dummy :fetcher github :repo "hlissner/does-not-exist")
            (enfer-noquelpa-dummy :fetcher github :repo "hlissner/does-not-exist-3")
            (enfer-new-quelpa-dummy :fetcher github :repo "hlissner/does-not-exist-2"))
          package-selected-packages (mapcar #'car enfer-packages)))

  (describe "package-backend"
    (it "determines the correct backend of a package"
      (expect (enfer-package-backend 'enfer-dummy) :to-be 'elpa)
      (expect (enfer-package-backend 'enfer-quelpa-dummy) :to-be 'quelpa)
      (expect (enfer-package-backend 'org) :to-be 'emacs))
    (it "errors out if package isn't installed"
      (expect (enfer-package-backend 'xyz) :to-throw)))

  (describe "package-outdated-p (elpa)"
    (it "detects outdated ELPA packages and returns both versions"
      (expect (enfer-package-outdated-p 'enfer-dummy)
              :to-equal '(enfer-dummy (20160405 1234) (20170405 1234))))
    (it "ignores up-to-date ELPA packages"
      (expect (enfer-package-outdated-p 'enfer-uptodate-dummy) :to-be nil))

    (it "detects outdated QUELPA packages and returns both versions"
      (expect (enfer-package-outdated-p 'enfer-quelpa-dummy)
              :to-equal '(enfer-quelpa-dummy (20160405 1234) (20170405 1234))))
    (it "ignores up-to-date QUELPA packages"
      (expect (enfer-package-outdated-p 'enfer-uptodate-dummy) :to-be nil))

    (it "returns nil if package isn't installed"
      (expect (enfer-package-outdated-p 'xyz) :to-be nil)))

  (describe "get-packages"
    (before-all
      ;; In addition to `package-installed-p', `enfer-package-installed-p' does
      ;; file existence checks which won't work here, so we simplify it
      (spy-on #'enfer-package-installed-p :and-call-fake #'package-installed-p))

    (it "returns all packages"
      (expect (mapcar #'car (enfer-find-packages))
              :to-have-same-items-as
              (mapcar #'car enfer-packages)))
    (it "returns only disabled packages"
      (expect (mapcar #'car (enfer-find-packages :disabled t))
              :to-have-same-items-as
              '(enfer-disabled-dummy enfer-disabled-private-dummy)))
    (it "returns only non-disabled packages"
      (expect (mapcar #'car (enfer-find-packages :disabled nil))
              :to-have-same-items-as
              '(enfer-dummy enfer-uptodate-dummy enfer-quelpa-dummy enfer-missing-dummy enfer-noquelpa-dummy enfer-private-dummy)))
    (it "returns only installed packages"
      (expect (mapcar #'car (enfer-find-packages :disabled nil :installed t))
              :to-have-same-items-as
              '(enfer-dummy enfer-uptodate-dummy enfer-quelpa-dummy enfer-noquelpa-dummy)))
    (it "returns only non-installed packages"
      (expect (mapcar #'car (enfer-find-packages :disabled nil :installed nil))
              :to-have-same-items-as
              '(enfer-missing-dummy enfer-private-dummy)))
    (it "returns only private packages"
      (expect (mapcar #'car (enfer-find-packages :private t))
              :to-have-same-items-as
              '(enfer-private-dummy enfer-disabled-private-dummy)))
    (it "returns only disabled and private packages"
      (expect (mapcar #'car (enfer-find-packages :disabled t :private t))
              :to-have-same-items-as
              '(enfer-disabled-private-dummy))))

  (describe "get-orphaned-packages"
    (it "returns orphaned packages"
      (expect (enfer-get-orphaned-packages) :to-contain 'enfer-unwanted-dummy))
    (it "returns packages that have changed backends"
      (expect (enfer-get-orphaned-packages) :to-contain 'enfer-noquelpa-dummy)))

  (describe "get-missing-packages"
    (it "returns packages that haven't been installed"
      (expect (mapcar #'car (enfer-get-missing-packages))
              :to-contain 'enfer-missing-dummy))
    (it "returns packages that have changed backends"
      (expect (mapcar #'car (enfer-get-missing-packages))
              :to-contain 'enfer-noquelpa-dummy))))
