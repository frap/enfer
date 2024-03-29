;; -*- no-byte-compile: t; -*-
;;; core/test/autoload-debug.el

(def-test! what-face
  (insert (propertize "Hello " 'face 'font-lock-keyword-face))
  (insert "world")

  (should (equal (enfer/what-face (point-min)) '((font-lock-keyword-face) ())))
  (should-not (enfer/what-face (point-max))))

(def-test! what-face-overlays
  (insert "Hello world")
  (let ((ov (make-overlay 1 6)))
    (overlay-put ov 'face 'font-lock-keyword-face))

  (should (equal (enfer/what-face (point-min)) '(() (font-lock-keyword-face))))
  (should-not (enfer/what-face (point-max))))
