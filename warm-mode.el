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

(defgroup warm-mode nil
  "Warm colors for nighttime coding."
  :group 'faces
  :prefix "warm-mode-")

(defvar warm-mode)

(defun warm-mode--set-and-refresh (sym val min max)
  "Set SYM to VAL clamped between MIN and MAX, and refresh if active."
  (set-default sym (max min (min max val)))
  ;; Runs at load time too, before the mode variable exists.
  (when (bound-and-true-p warm-mode)
    (warm-mode--remove t)
    (warm-mode--apply)))

(defcustom warm-mode-warmth 0.2
  "Intensity of the warm color shift.
Value should be between 0.0 (no warmth) and 0.5 (very warm)."
  :type 'float
  :set (lambda (sym val) (warm-mode--set-and-refresh sym val 0.0 0.5)))

(defcustom warm-mode-dim 0.9
  "Brightness multiplier.
Value should be between 0.5 (very dim) and 1.0 (no dimming)."
  :type 'float
  :set (lambda (sym val) (warm-mode--set-and-refresh sym val 0.5 1.0)))

(defvar warm-mode--color-cache (make-hash-table :test #'equal)
  "Cache mapping an original color string to its warmed color string.")

(defvar warm-mode--faces (make-hash-table :test #'eq)
  "Faces warmed so far, each mapped to (FG BG WARM-FG WARM-BG).
FG and BG are the colors the face had before warming, WARM-FG and
WARM-BG the colors we set.  Comparing a face against the latter tells
whether it has been changed behind our back since.")

(defun warm-mode--warm-color (color)
  "Return a warmer, dimmer version of COLOR, or nil if it cannot be parsed."
  (when (stringp color)
    (or (gethash color warm-mode--color-cache)
        (pcase (color-name-to-rgb color)
          (`(,r ,g ,b)
           (puthash color
                    (color-rgb-to-hex
                     ;; Red: boost slightly, then dim
                     (min 1.0 (* (+ r (* warm-mode-warmth 0.4)) warm-mode-dim))
                     ;; Green: just dim
                     (* g warm-mode-dim)
                     ;; Blue: reduce, then dim
                     (* (max 0.0 (- b warm-mode-warmth)) warm-mode-dim)
                     2)
                    warm-mode--color-cache))))))

(defun warm-mode--warm-face (face)
  "Warm FACE from its original colors.
A channel still showing the color we last set keeps its saved
original, anything else was changed since and becomes the new
original.  Calling this repeatedly never warms a face twice."
  (let* ((entry (gethash face warm-mode--faces))
         (fg (face-foreground face nil nil))
         (bg (face-background face nil nil))
         (orig-fg (if (and entry (equal fg (nth 2 entry))) (nth 0 entry) fg))
         (orig-bg (if (and entry (equal bg (nth 3 entry))) (nth 1 entry) bg))
         (warm-fg (warm-mode--warm-color orig-fg))
         (warm-bg (warm-mode--warm-color orig-bg)))
    (when (or warm-fg warm-bg)
      (puthash face
               (list orig-fg orig-bg (or warm-fg orig-fg) (or warm-bg orig-bg))
               warm-mode--faces)
      (when (and warm-fg (not (equal fg warm-fg)))
        (set-face-foreground face warm-fg))
      (when (and warm-bg (not (equal bg warm-bg)))
        (set-face-background face warm-bg)))))

(defun warm-mode--apply ()
  "Warm every face, then redisplay."
  (let ((inhibit-redisplay t))
    (mapc #'warm-mode--warm-face (face-list)))
  (redisplay t))

(defun warm-mode--remove (&optional no-redisplay)
  "Restore the original colors of every warmed face.
Channels changed since we warmed them are left alone.  When
NO-REDISPLAY is non-nil, skip forcing a redisplay."
  (let ((inhibit-redisplay t))
    (maphash
     (lambda (face entry)
       (pcase-let ((`(,fg ,bg ,warm-fg ,warm-bg) entry))
         (when (facep face)
           (when (and (not (equal fg warm-fg))
                      (equal (face-foreground face nil nil) warm-fg))
             (set-face-foreground face fg))
           (when (and (not (equal bg warm-bg))
                      (equal (face-background face nil nil) warm-bg))
             (set-face-background face bg)))))
     warm-mode--faces))
  (clrhash warm-mode--faces)
  (clrhash warm-mode--color-cache)
  (unless no-redisplay (redisplay t)))

(defun warm-mode--around-theme (fn &rest args)
  "Restore faces, call FN with ARGS to toggle a theme, then rewarm.
Themes stack, so faces the theme does not touch would otherwise be
warmed a second time and their originals lost."
  (if warm-mode
      (progn
        (warm-mode--remove t)
        (unwind-protect (apply fn args)
          (warm-mode--apply)))
    (apply fn args)))

(defun warm-mode--after-spec-set (face &rest _)
  "Warm FACE right after `defface' or `custom-set-faces' colors it."
  (when warm-mode (warm-mode--warm-face face)))

(defun warm-mode--on-first-frame (frame)
  "Warm faces on FRAME, the first real frame of a daemon."
  (remove-hook 'after-make-frame-functions #'warm-mode--on-first-frame)
  (with-selected-frame frame (warm-mode--apply)))

;;;###autoload
(define-minor-mode warm-mode
  "Global minor mode that warms all colors for nighttime coding.
Reduces blue light and slightly dims colors across all faces."
  :global t
  :lighter " Warm"
  (if warm-mode
      (progn
        (advice-add 'enable-theme :around #'warm-mode--around-theme)
        (advice-add 'disable-theme :around #'warm-mode--around-theme)
        (advice-add 'face-spec-set :after #'warm-mode--after-spec-set)
        (if (and (daemonp) (eq (selected-frame) terminal-frame))
            ;; The daemon's initial frame has no real colors to read,
            ;; wait for the first client frame instead.
            (add-hook 'after-make-frame-functions #'warm-mode--on-first-frame)
          (warm-mode--apply)))
    (advice-remove 'enable-theme #'warm-mode--around-theme)
    (advice-remove 'disable-theme #'warm-mode--around-theme)
    (advice-remove 'face-spec-set #'warm-mode--after-spec-set)
    (remove-hook 'after-make-frame-functions #'warm-mode--on-first-frame)
    (warm-mode--remove)))

(provide 'warm-mode)
;;; warm-mode.el ends here
