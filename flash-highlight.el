;;; flash-highlight.el --- Highlighting for flash -*- lexical-binding: t -*-

;; Copyright (C) 2025 Vadim Pavlov
;; Author: Vadim Pavlov <https://github.com/Prgebish>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Visual feedback using overlays: backdrop, match highlighting, labels.

;;; Code:

(require 'flash-state)

(declare-function flash-match-pos-value "flash-state" (match))
(declare-function flash-match-end-pos-value "flash-state" (match))
(declare-function flash-match-buffer-live "flash-state" (match))

;;; Faces
;; Default faces inherit from theme, rainbow uses hardcoded Tailwind colors

(defface flash-label
  '((((background light))
     :background "#3b82f6" :foreground "#ffffff" :weight bold)
    (((background dark))
     :background "#fecdd3" :foreground "#881337" :weight bold))
  "Face for jump labels.
Blue on light, light pink on dark (Tailwind blue-500, rose-200/900).
Customize with \\[customize-face] or `custom-set-faces'."
  :group 'flash)

(defface flash-match
  '((t :underline t))
  "Face for search matches.
Subtle underline (uses foreground color) to mark matched text."
  :group 'flash)

(defface flash-backdrop
  '((t :inherit shadow))
  "Face for backdrop effect.
Inherits from `shadow' (like flash.nvim's FlashBackdrop → Comment)."
  :group 'flash)

;;; Rainbow palette
;; Dynamic Tailwind CSS colors matching flash.nvim's rainbow module.
;; Shade controlled by `flash-rainbow-shade' (1-9).

(defvar flash--rainbow-color-names
  '(red amber lime green teal cyan blue violet fuchsia rose)
  "Color names for rainbow labels, same 10 as flash.nvim.")

(defconst flash--tailwind-palette
  '((red     . ["#fef2f2" "#fee2e2" "#fecaca" "#fca5a5" "#f87171" "#ef4444" "#dc2626" "#b91c1c" "#991b1b" "#7f1d1d" "#450a0a"])
    (amber   . ["#fffbeb" "#fef3c7" "#fde68a" "#fcd34d" "#fbbf24" "#f59e0b" "#d97706" "#b45309" "#92400e" "#78350f" "#451a03"])
    (lime    . ["#f7fee7" "#ecfccb" "#d9f99d" "#bef264" "#a3e635" "#84cc16" "#65a30d" "#4d7c0f" "#3f6212" "#365314" "#1a2e05"])
    (green   . ["#f0fdf4" "#dcfce7" "#bbf7d0" "#86efac" "#4ade80" "#22c55e" "#16a34a" "#15803d" "#166534" "#14532d" "#052e16"])
    (teal    . ["#f0fdfa" "#ccfbf1" "#99f6e4" "#5eead4" "#2dd4bf" "#14b8a6" "#0d9488" "#0f766e" "#115e59" "#134e4a" "#042f2e"])
    (cyan    . ["#ecfeff" "#cffafe" "#a5f3fc" "#67e8f9" "#22d3ee" "#06b6d4" "#0891b2" "#0e7490" "#155e75" "#164e63" "#083344"])
    (blue    . ["#eff6ff" "#dbeafe" "#bfdbfe" "#93c5fd" "#60a5fa" "#3b82f6" "#2563eb" "#1d4ed8" "#1e40af" "#1e3a8a" "#172554"])
    (violet  . ["#f5f3ff" "#ede9fe" "#ddd6fe" "#c4b5fd" "#a78bfa" "#8b5cf6" "#7c3aed" "#6d28d9" "#5b21b6" "#4c1d95" "#2e1065"])
    (fuchsia . ["#fdf4ff" "#fae8ff" "#f5d0fe" "#f0abfc" "#e879f9" "#d946ef" "#c026d3" "#a21caf" "#86198f" "#701a75" "#4a044e"])
    (rose    . ["#fff1f2" "#ffe4e6" "#fecdd3" "#fda4af" "#fb7185" "#f43f5e" "#e11d48" "#be123c" "#9f1239" "#881337" "#4c0519"]))
  "Tailwind CSS color palette.
Each entry is (COLOR . [shade-50 shade-100 ... shade-900 shade-950]).
Indices: 0=50, 1=100, 2=200, ..., 9=900, 10=950.")

;;; Configuration (set by flash.el)

(defvar flash-backdrop t)
(defvar flash-rainbow nil)
(defvar flash-rainbow-shade 2)
(defvar flash-highlight-matches t)
(defvar flash-label-position 'overlay)

;;; Highlight Functions

(defun flash-highlight-update (state)
  "Update highlighting for STATE.
Reuses existing match/label overlays where possible.
Backdrop is created once and reused across updates."
  ;; Ensure backdrop exists (no-op after first call)
  (when flash-backdrop
    (flash--ensure-backdrop state))

  ;; Reconcile match/label overlays with current match set.
  (flash--reconcile-overlays state))

(defun flash--overlay-pools (state)
  "Return reusable overlay pools from STATE as (MATCHES . LABELS)."
  (let (match-overlays label-overlays)
    (dolist (ov (flash-state-overlays state))
      (if (eq (overlay-get ov 'flash-kind) 'label)
          (push ov label-overlays)
        (push ov match-overlays)))
    (cons (nreverse match-overlays) (nreverse label-overlays))))

(defun flash--reset-overlay-decoration (ov)
  "Reset display string properties on overlay OV."
  (overlay-put ov 'before-string nil)
  (overlay-put ov 'after-string nil)
  (overlay-put ov 'display nil))

(defun flash--configure-match-overlay (ov buf pos end-pos)
  "Configure OV as a match overlay in BUF for POS..END-POS."
  (move-overlay ov pos end-pos buf)
  (flash--reset-overlay-decoration ov)
  (overlay-put ov 'face 'flash-match)
  (overlay-put ov 'flash t)
  (overlay-put ov 'flash-kind 'match)
  (overlay-put ov 'priority 100))

(defun flash--configure-label-overlay (ov state buf pos end-pos label face position)
  "Configure OV as a label overlay for match and return OV.
STATE provides current label prefix.
BUF, POS, END-POS locate the match.
LABEL, FACE, and POSITION control displayed label."
  (let* ((prefix (flash-state-label-prefix state))
         (display-label (if (and prefix (string-prefix-p prefix label))
                            (substring label (length prefix))
                          label))
         (label-str (propertize display-label 'face face))
         (max-end (1+ (with-current-buffer buf (point-max)))))
    (with-current-buffer buf
      (pcase position
        ('after
         (move-overlay ov end-pos end-pos buf)
         (flash--reset-overlay-decoration ov)
         (overlay-put ov 'after-string label-str))
        ('before
         (move-overlay ov pos pos buf)
         (flash--reset-overlay-decoration ov)
         (overlay-put ov 'before-string label-str))
        ('overlay
         (move-overlay ov pos (min (1+ pos) max-end) buf)
         (flash--reset-overlay-decoration ov)
         (overlay-put ov 'display label-str))
        ('pre-overlay
         (if (and (> pos (point-min))
                  (not (eq (char-before pos) ?\n)))
             (move-overlay ov (1- pos) pos buf)
           (move-overlay ov pos (min (1+ pos) max-end) buf))
         (flash--reset-overlay-decoration ov)
         (overlay-put ov 'display label-str))
        ('eol
         (save-excursion
           (goto-char pos)
           (let ((eol (line-end-position)))
             (move-overlay ov eol eol buf)))
         (flash--reset-overlay-decoration ov)
         (overlay-put ov 'after-string (concat " " label-str)))
        (_
         (move-overlay ov end-pos end-pos buf)
         (flash--reset-overlay-decoration ov)
         (overlay-put ov 'after-string label-str))))
    (overlay-put ov 'face nil)
    (overlay-put ov 'flash t)
    (overlay-put ov 'flash-kind 'label)
    (overlay-put ov 'priority 200)
    ov))

(defun flash--reconcile-overlays (state)
  "Update overlays in STATE by reusing existing overlay objects."
  (let* ((pools (flash--overlay-pools state))
         (match-pool (car pools))
         (label-pool (cdr pools))
         new-overlays
         (index 0))
    (dolist (match (flash-state-matches state))
      (let* ((pos (flash-match-pos-value match))
             (end-pos (flash-match-end-pos-value match))
             (label (flash-match-label match))
             (prefix (flash-state-label-prefix state))
             (fold (flash-match-fold match))
             (show-label (and label
                              (or (not prefix)
                                  (string-prefix-p prefix label))))
             (face (when show-label (flash--get-label-face index)))
             (buf (flash-match-buffer-live match)))
        (when (and (buffer-live-p buf)
                   (integerp pos)
                   (integerp end-pos))
          (with-current-buffer buf
            (let ((max-end (1+ (point-max))))
              (setq pos (max (point-min) pos))
              (setq end-pos (min max-end end-pos))))
          (unless fold
            (when (and flash-highlight-matches
                       (> end-pos pos))
              (let ((ov (or (pop match-pool)
                            (make-overlay pos end-pos buf))))
                (flash--configure-match-overlay ov buf pos end-pos)
                (push ov new-overlays)))
            (when show-label
              (let ((ov (or (pop label-pool)
                            (make-overlay pos pos buf))))
                (flash--configure-label-overlay
                 ov state buf pos end-pos label face flash-label-position)
                (push ov new-overlays)))))
        (when label
          (setq index (1+ index)))))
    ;; Delete overlays no longer needed.
    (mapc #'delete-overlay match-pool)
    (mapc #'delete-overlay label-pool)
    (setf (flash-state-overlays state) (nreverse new-overlays))))

(defun flash-highlight-clear (state)
  "Remove match/label overlays from STATE.
Backdrop overlays are preserved for reuse."
  (mapc #'delete-overlay (flash-state-overlays state))
  (setf (flash-state-overlays state) nil))

(defun flash-highlight-clear-all (state)
  "Remove all overlays from STATE, including backdrop."
  (flash-highlight-clear state)
  (mapc #'delete-overlay (flash-state-backdrop-overlays state))
  (setf (flash-state-backdrop-overlays state) nil))

(defun flash--ensure-backdrop (state)
  "Ensure backdrop overlays in STATE track the current window view.
Existing overlays are reused and moved to current `window-start' / `window-end'."
  (let ((existing (make-hash-table :test 'eq))
        new-overlays)
    ;; Index existing overlays by source window and drop invalid ones.
    (dolist (ov (flash-state-backdrop-overlays state))
      (let ((win (overlay-get ov 'flash-window)))
        (if (and (overlay-buffer ov)
                 (window-live-p win))
            (puthash win ov existing)
          (delete-overlay ov))))
    ;; Ensure one backdrop overlay per live window and move it to current range.
    (dolist (win (flash-state-windows state))
      (when (window-live-p win)
        (let* ((buf (window-buffer win))
               (start (window-start win))
               (end (window-end win t))
               (ov (or (gethash win existing)
                       (make-overlay start end buf))))
          (move-overlay ov start end buf)
          (overlay-put ov 'face 'flash-backdrop)
          (overlay-put ov 'flash t)
          (overlay-put ov 'flash-kind 'backdrop)
          (overlay-put ov 'flash-window win)
          (overlay-put ov 'priority 0)
          (push ov new-overlays)
          (remhash win existing))))
    ;; Drop leftover overlays for windows no longer in scope.
    (maphash (lambda (_win ov)
               (delete-overlay ov))
             existing)
    (setf (flash-state-backdrop-overlays state)
          (nreverse new-overlays))))

(defun flash--rainbow-face (index)
  "Compute face plist for rainbow label at INDEX.
Uses `flash-rainbow-shade' to select brightness from Tailwind palette.
Foreground is chosen for contrast: dark text on light bg, light on dark."
  (let* ((shade (if (bound-and-true-p flash-rainbow-shade)
                    flash-rainbow-shade
                  5))
         (color-name (nth (mod index (length flash--rainbow-color-names))
                          flash--rainbow-color-names))
         (palette (cdr (assq color-name flash--tailwind-palette)))
         (bg (aref palette shade))
         (fg-index (cond
                    ((< shade 5) 9)   ; shade 900
                    ((= shade 5) 10)  ; shade 950
                    (t 0)))           ; shade 50
         (fg (aref palette fg-index)))
    (list :background bg :foreground fg :weight 'bold)))

(defun flash--get-label-face (index)
  "Get face for label at INDEX.
Returns rainbow face if `flash-rainbow' is enabled, otherwise default."
  (if (bound-and-true-p flash-rainbow)
      (flash--rainbow-face index)
    'flash-label))

(provide 'flash-highlight)
;;; flash-highlight.el ends here
