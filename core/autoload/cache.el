;;; ../core/autoload/cache.el -*- lexical-binding: t; -*-

;; This little library thinly wraps around persistent-soft (which is a pcache
;; wrapper, how about that). It has three purposes:
;;
;; + To encapsulate the cache backend (persistent-soft/pcache in this case), in
;;   case it needs to change.
;; + To provide `enfer-cache-persist': a mechanism for easily persisting
;;   variables across Emacs sessions.
;; + To lazy-load persistent-soft until it is really needed.
;;
;; Like persistent-soft, caches assume a 2-tier structure, where all caches are
;; namespaced by location.

(defvar enfer-cache-alists '(t)
  "An alist of alists, containing lists of variables for the enfer cache library
to persist across Emacs sessions.")

(defvar enfer-cache-location 'enfer
  "The default location for cache files. This symbol is translated into a file
name under `pcache-directory' (by default a subdirectory under
`enfer-cache-dir'). One file may contain multiple cache entries.")

(defun enfer|save-persistent-cache ()
  "Hook to run when an Emacs session is killed. Saves all persisted variables
listed in `enfer-cache-alists' to files."
  (dolist (alist (butlast enfer-cache-alists 1))
    (cl-loop with key = (car alist)
             for var in (cdr alist)
             if (symbol-value var)
             do (enfer-cache-set var it nil key))))
(add-hook 'kill-emacs-hook #'enfer|save-persistent-cache)


;;
;; Library

;;;###autoload
(defmacro with-cache! (location &rest body)
  "Runs BODY with a different default `enfer-cache-location'."
  (declare (indent defun))
  `(let ((enfer-cache-location ',location))
     ,@body))

;;;###autoload
(defun enfer-cache-persist (location variables)
  "Persist VARIABLES (list of symbols) in LOCATION (symbol).

This populates these variables with cached values, if one exists, and saves them
to file when Emacs quits.

Warning: this is incompatible with buffer-local variables."
  (dolist (var variables)
    (when (enfer-cache-exists var location)
      (set var (enfer-cache-get var location))))
  (setf (alist-get location enfer-cache-alists)
        (append variables (cdr (assq location enfer-cache-alists)))))

;;;###autoload
(defun enfer-cache-desist (location &optional variables)
  "Unregisters VARIABLES (list of symbols) in LOCATION (symbol) from
`enfer-cache-alists', thus preventing them from being saved between sessions.
Does not affect the actual variables themselves or their values."
  (if variables
      (setf (alist-get location enfer-cache-alists)
            (cl-set-difference (cdr (assq location enfer-cache-alists))
                               variables))
    (delq (assq location enfer-cache-alists)
          enfer-cache-alists)))

;;;###autoload
(defun enfer-cache-get (key &optional location)
  "Retrieve KEY from LOCATION (defaults to `enfer-cache-location'), if it exists
and hasn't expired."
  (persistent-soft-fetch
   key (symbol-name (or location enfer-cache-location))))

;;;###autoload
(defun enfer-cache-set (key value &optional ttl location)
  "Set KEY to VALUE in the cache. TTL is the time (in seconds) until this cache
entry expires. LOCATION is the super-key to store this cache item under; the
default is `enfer-cache-location'. "
  (persistent-soft-store
   key value
   (symbol-name (or location enfer-cache-location)) ttl))

;;;###autoload
(defun enfer-cache-exists (key &optional location)
  "Returns t if KEY exists at LOCATION (defaults to `enfer-cache-location')."
  (persistent-soft-exists-p key (or location enfer-cache-location)))

;;;###autoload
(defun enfer-cache-clear (&optional location)
  "Clear a cache LOCATION (defaults to `enfer-cache-location')."
  (persistent-soft-flush (or location enfer-cache-location)))
