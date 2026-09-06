;;; warm-mode-test.el --- Tests for warm-mode -*- lexical-binding: t -*-

;;; Commentary:

;; Run with:
;;   emacs -Q --batch -l warm-mode.el -l warm-mode-test.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'warm-mode)

(defface warm-mode-test-face
  '((t :foreground "#4060ff" :background "#ffffff"))
  "Face used by the warm-mode tests.")

(deftheme warm-mode-test-theme "Theme used by the warm-mode tests.")
(custom-theme-set-faces
 'warm-mode-test-theme
 '(warm-mode-test-face ((t :foreground "#00ff00"))))

(defmacro warm-mode-test--with-mode (&rest body)
  "Run BODY with `warm-mode' on and the test face reset, then turn it off."
  `(unwind-protect
       (progn
         (set-face-foreground 'warm-mode-test-face "#4060ff")
         (set-face-background 'warm-mode-test-face "#ffffff")
         (warm-mode 1)
         ,@body)
     (warm-mode -1)))

(ert-deftest warm-mode-test-color-shift ()
  "Warming raises red, lowers green and blue, and rejects non-colors."
  (let ((warm-mode-warmth 0.2)
        (warm-mode-dim 0.9))
    (clrhash warm-mode--color-cache)
    (pcase-let ((`(,r ,g ,b)
                 (color-name-to-rgb (warm-mode--warm-color "#8080ff"))))
      (should (> r 0.5))
      (should (< g 0.5))
      (should (< b 0.75)))
    (should-not (warm-mode--warm-color "unspecified-fg"))
    (should-not (warm-mode--warm-color nil))))

(ert-deftest warm-mode-test-round-trip ()
  "Enabling warms the face and disabling restores it exactly."
  (warm-mode-test--with-mode
   (should-not (equal (face-foreground 'warm-mode-test-face) "#4060ff"))
   (should-not (equal (face-background 'warm-mode-test-face) "#ffffff")))
  (should (equal (face-foreground 'warm-mode-test-face) "#4060ff"))
  (should (equal (face-background 'warm-mode-test-face) "#ffffff")))

(ert-deftest warm-mode-test-idempotent ()
  "Re-applying or re-enabling never warms a face a second time."
  (warm-mode-test--with-mode
   (let ((once (face-foreground 'warm-mode-test-face)))
     (warm-mode--apply)
     (warm-mode 1)
     (should (equal (face-foreground 'warm-mode-test-face) once)))))

(ert-deftest warm-mode-test-theme-change ()
  "A theme enabled under warm-mode is warmed once, and disabling it restores."
  (warm-mode-test--with-mode
   (let ((warm-blue (face-foreground 'warm-mode-test-face))
         (warm-green (warm-mode--warm-color "#00ff00")))
     (enable-theme 'warm-mode-test-theme)
     (should (equal (face-foreground 'warm-mode-test-face) warm-green))
     (disable-theme 'warm-mode-test-theme)
     (should (equal (face-foreground 'warm-mode-test-face) warm-blue))))
  (should (equal (face-foreground 'warm-mode-test-face) "#4060ff")))

(ert-deftest warm-mode-test-runtime-change-kept ()
  "A face recolored under warm-mode is warmed from, and restored to, its new color."
  (warm-mode-test--with-mode
   (set-face-foreground 'warm-mode-test-face "#00ff00")
   (warm-mode--apply)
   (should (equal (face-foreground 'warm-mode-test-face)
                  (warm-mode--warm-color "#00ff00"))))
  (should (equal (face-foreground 'warm-mode-test-face) "#00ff00")))

(ert-deftest warm-mode-test-new-face-warmed ()
  "A face defined under warm-mode is warmed immediately."
  (warm-mode-test--with-mode
   (defface warm-mode-test-late-face '((t :foreground "#0000ff")) "Late face.")
   (should (equal (face-foreground 'warm-mode-test-late-face)
                  (warm-mode--warm-color "#0000ff")))))

(provide 'warm-mode-test)
;;; warm-mode-test.el ends here
