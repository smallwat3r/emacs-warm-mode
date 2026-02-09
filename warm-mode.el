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
;;   `warm-mode-warmth'  - intensity of warm shift (0.0 to 0.5)
;;   `warm-mode-dim'     - brightness multiplier (0.5 to 1.0)

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
    (warm-mode--remove t)
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
            ;; Convert color name or hex to RGB floats (0.0-1.0)
            (let ((rgb (color-name-to-rgb color)))
              (when rgb
                (let* ((r (car rgb))
                       (g (cadr rgb))
                       (b (caddr rgb))
                       ;; Red: boost slightly, then dim
                       (r-warm (min 1.0 (* (+ r (* warm-mode-warmth 0.4))
                                           warm-mode-dim)))
                       ;; Green: just dim
                       (g-warm (* g warm-mode-dim))
                       ;; Blue: reduce, then dim
                       (b-warm (* (max 0.0 (- b warm-mode-warmth))
                                  warm-mode-dim))
                       (result (color-rgb-to-hex r-warm g-warm b-warm 2)))
                  (puthash color result warm-mode--color-cache)
                  result)))
          (error nil)))))

(defun warm-mode--apply ()
  "Apply warm color transformation to all faces."
  (setq warm-mode--color-cache (make-hash-table :test 'equal :size 128)
        warm-mode--original-faces nil)
  (let ((inhibit-redisplay t))
    (dolist (face (face-list))
      (let* ((fg (face-foreground face nil nil))
             (bg (face-background face nil nil))
             (fg-warm (and fg (stringp fg) (warm-mode--warm-color fg)))
             (bg-warm (and bg (stringp bg) (warm-mode--warm-color bg))))
        (when (or fg-warm bg-warm)
          (push (list face fg bg) warm-mode--original-faces)
          (when fg-warm (set-face-foreground face fg-warm))
          (when bg-warm (set-face-background face bg-warm))))))
  (redisplay t))

(defun warm-mode--remove (&optional no-redisplay)
  "Restore original face colors.
When NO-REDISPLAY is non-nil, skip forcing a redisplay."
  (when warm-mode--original-faces
    (let ((inhibit-redisplay t))
      (dolist (entry warm-mode--original-faces)
        (let ((face (car entry))
              (fg (cadr entry))
              (bg (caddr entry)))
          (when (facep face)
            (set-face-foreground face fg)
            (set-face-background face bg)))))
    (unless no-redisplay (redisplay t)))
  (setq warm-mode--color-cache nil
        warm-mode--original-faces nil))

(defun warm-mode--on-theme-change (&rest _)
  "Reapply warm colors after a theme change without toggling the mode."
  (when (bound-and-true-p warm-mode)
    (setq warm-mode--color-cache nil
          warm-mode--original-faces nil)
    (warm-mode--remove t)
    (warm-mode--apply)))

(defun warm-mode--refresh ()
  "Refresh warm colors if mode is active."
  (when (bound-and-true-p warm-mode)
    (warm-mode--remove t)
    (warm-mode--apply)))

(defvar warm-mode--refresh-timer nil
  "Timer for debounced face refresh after package loads.")

(defvar warm-mode--hooks-registered nil
  "Whether `warm-mode' has registered its hooks/advice.")

(defun warm-mode--register-hooks ()
  "Register advice and hooks once."
  (unless warm-mode--hooks-registered
    (advice-add 'load-theme :after #'warm-mode--on-theme-change)
    (add-hook 'after-load-functions #'warm-mode--refresh-soon)
    (setq warm-mode--hooks-registered t)))

(defun warm-mode--unregister-hooks ()
  "Remove advice and hooks if present."
  (when warm-mode--hooks-registered
    (remove-hook 'after-load-functions #'warm-mode--refresh-soon)
    (advice-remove 'load-theme #'warm-mode--on-theme-change)
    (setq warm-mode--hooks-registered nil)))

(defun warm-mode--refresh-soon (&rest _)
  "Schedule a debounced `warm-mode--refresh' after a file is loaded.
Hooks into `after-load-functions' which runs on `load' or `require'.
Deferred packages define faces on load that need warming.  Resets
the timer on each call so that rapid successive loads only trigger
a single refresh."
  (when (bound-and-true-p warm-mode)
    (if (memq warm-mode--refresh-timer timer-list)
        (timer-set-time warm-mode--refresh-timer
                        (time-add (current-time) 0.5))
      (setq warm-mode--refresh-timer
            (run-with-timer 0.5 nil #'warm-mode--refresh)))))

;;;###autoload
(define-minor-mode warm-mode
  "Global minor mode that warms all colors for nighttime coding.
Reduces blue light and slightly dims colors across all faces."
  :global t
  :lighter " Warm"
  (if warm-mode
      (unless warm-mode--original-faces
        (warm-mode--register-hooks)
        (warm-mode--apply))
    (warm-mode--unregister-hooks)
    (when (memq warm-mode--refresh-timer timer-list)
      (cancel-timer warm-mode--refresh-timer))
    (setq warm-mode--refresh-timer nil)
    (warm-mode--remove)))

(provide 'warm-mode)
;;; warm-mode.el ends here
