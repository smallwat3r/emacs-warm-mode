;;; warm-mode.el --- Warm colors for nighttime coding -*- lexical-binding: t -*-

;; Author: Matthieu Petiteau <mpetiteau.pro@gmail.com>
;; URL: https://github.com/smallwat3r/emacs-warm-mode
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces, convenience

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A global minor mode that warms all Emacs colors for nighttime coding.
;; Reduces blue light and slightly dims colors across all faces, working
;; with any theme.
;;
;; Usage:
;;   M-x warm-mode
;;
;; Customization:
;;   `warm-mode-warmth' - intensity of warm shift (0.0 to 0.5)
;;   `warm-mode-dim'    - brightness multiplier (0.5 to 1.0)

;;; Code:

(require 'color)

(defgroup warm nil
  "Warm colors for nighttime coding."
  :group 'faces
  :prefix "warm-mode-")

(defun warm-mode--set-and-refresh (sym val min max)
  "Set SYM to VAL clamped between MIN and MAX, and refresh if active."
  (set-default sym (max min (min max val)))
  (when (bound-and-true-p warm-mode)
    (warm-mode--remove)
    (warm-mode--apply)))

(defcustom warm-mode-warmth 0.2
  "Intensity of the warm color shift.
Value should be between 0.0 (no warmth) and 0.5 (very warm)."
  :type 'float
  :set (lambda (sym val) (warm-mode--set-and-refresh sym val 0.0 0.5))
  :group 'warm)

(defcustom warm-mode-dim 0.9
  "Brightness multiplier.
Value should be between 0.5 (very dim) and 1.0 (no dimming)."
  :type 'float
  :set (lambda (sym val) (warm-mode--set-and-refresh sym val 0.5 1.0))
  :group 'warm)

(defvar warm-mode--color-cache nil
  "Hash table caching color transformations.")

(defvar warm-mode--original-faces nil
  "Alist of original face colors before warm mode was applied.")

(defun warm-mode--warm-color (color)
  "Shift COLOR to be warmer (more red/orange, less blue)."
  (when (and color (stringp color) warm-mode--color-cache)
    (or (gethash color warm-mode--color-cache)
        (condition-case nil
            (let ((rgb (color-name-to-rgb color)))
              (when rgb
                (let* ((r (car rgb))
                       (g (cadr rgb))
                       (b (caddr rgb))
                       (result (color-rgb-to-hex
                                (min 1.0 (* (+ r (* warm-mode-warmth 0.4))
                                            warm-mode-dim))
                                (* g warm-mode-dim)
                                (* (max 0.0 (- b warm-mode-warmth))
                                   warm-mode-dim)
                                2)))
                  (puthash color result warm-mode--color-cache)
                  result)))
          (error nil)))))

(defun warm-mode--transform-face (face)
  "Transform a single FACE to warm colors."
  (let ((fg (face-foreground face nil nil))
        (bg (face-background face nil nil))
        warm)
    (when (and fg (stringp fg) (setq warm (warm-mode--warm-color fg)))
      (set-face-foreground face warm))
    (when (and bg (stringp bg) (setq warm (warm-mode--warm-color bg)))
      (set-face-background face warm))))

(defun warm-mode--store-face (face)
  "Store original colors for FACE."
  (let ((fg (face-foreground face nil nil))
        (bg (face-background face nil nil)))
    (when (or fg bg)
      (push (list face fg bg) warm-mode--original-faces))))

(defun warm-mode--apply ()
  "Apply warm color transformation to all faces."
  (setq warm-mode--color-cache (make-hash-table :test 'equal :size 200)
        warm-mode--original-faces nil)
  (let ((inhibit-redisplay t)
        (faces (face-list)))
    (mapc #'warm-mode--store-face faces)
    (mapc #'warm-mode--transform-face faces))
  (redisplay t))

(defun warm-mode--restore-face (entry)
  "Restore original colors for face from ENTRY."
  (let ((face (car entry))
        (fg (cadr entry))
        (bg (caddr entry)))
    (when (facep face)
      (set-face-foreground face fg)
      (set-face-background face bg))))

(defun warm-mode--remove ()
  "Restore original face colors."
  (when warm-mode--original-faces
    (let ((inhibit-redisplay t))
      (mapc #'warm-mode--restore-face warm-mode--original-faces))
    (redisplay t))
  (setq warm-mode--color-cache nil
        warm-mode--original-faces nil))

(defun warm-mode--on-theme-change (&rest _)
  "Disable warm mode when theme changes."
  (when (bound-and-true-p warm-mode)
    (setq warm-mode--color-cache nil
          warm-mode--original-faces nil)
    (warm-mode -1)
    (message "Warm mode disabled (theme changed)")))

;;;###autoload
(define-minor-mode warm-mode
  "Global minor mode that warms all colors for nighttime coding.
Reduces blue light and slightly dims colors across all faces."
  :global t
  :lighter " Warm"
  (if warm-mode
      (progn
        (advice-add 'load-theme :before #'warm-mode--on-theme-change)
        (warm-mode--apply))
    (advice-remove 'load-theme #'warm-mode--on-theme-change)
    (warm-mode--remove)))

(provide 'warm-mode)
;;; warm-mode.el ends here
