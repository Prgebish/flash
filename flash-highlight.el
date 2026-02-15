;;; flash-highlight.el --- Highlighting for flash -*- lexical-binding: t -*-

;; Copyright (C) 2025 Vadim Pavlov
;; Author: Vadim Pavlov <https://github.com/Prgebish>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Visual feedback using overlays: backdrop, match highlighting, labels.

;;; Code:

(require 'flash-state)

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

(defvar flash-backdrop)
(defvar flash-rainbow)
(defvar flash-rainbow-shade)
(defvar flash-highlight-matches)
(defvar flash-label-position)

;;; Highlight Functions

(defun flash-highlight-update (state)
  "Update highlighting for STATE.
Clears old overlays and creates new ones for backdrop, matches, and labels."
  ;; Remove old overlays
  (flash-highlight-clear state)

  ;; Backdrop
  (when flash-backdrop
    (flash--highlight-backdrop state))

  ;; Matches and labels
  (let ((index 0))
    (dolist (match (flash-state-matches state))
      (flash--highlight-match state match index)
      (when (flash-match-label match)
        (setq index (1+ index))))))

(defun flash-highlight-clear (state)
  "Remove all overlays from STATE."
  (mapc #'delete-overlay (flash-state-overlays state))
  (setf (flash-state-overlays state) nil))

(defun flash--highlight-backdrop (state)
  "Add backdrop overlay to all windows in STATE."
  (dolist (win (flash-state-windows state))
    (when (window-live-p win)
      (with-current-buffer (window-buffer win)
        (let ((ov (make-overlay (window-start win) (window-end win t))))
          (overlay-put ov 'face 'flash-backdrop)
          (overlay-put ov 'flash t)
          (overlay-put ov 'priority 0)
          (push ov (flash-state-overlays state)))))))

(defun flash--highlight-match (state match index)
  "Add overlays for MATCH to STATE.
INDEX is used to select rainbow color when `flash-rainbow' is enabled."
  (let* ((pos (flash-match-pos match))
         (end-pos (flash-match-end-pos match))
         (label (flash-match-label match))
         (prefix (flash-state-label-prefix state))
         (fold (flash-match-fold match))
         ;; Skip labels that don't match current prefix
         (show-label (and label
                          (or (not prefix)
                              (string-prefix-p prefix label))))
         (face (when show-label (flash--get-label-face index)))
         (buf (marker-buffer pos))
         (position flash-label-position))
    (when buf
      (with-current-buffer buf
        (cond
         ;; Folded: no highlighting (match is invisible)
         (fold nil)
         ;; Normal: underline match and show label
         (t
          (when flash-highlight-matches
            (let ((ov (make-overlay pos end-pos)))
              (overlay-put ov 'face 'flash-match)
              (overlay-put ov 'flash t)
              (overlay-put ov 'priority 100)
              (push ov (flash-state-overlays state))))
          (when show-label
            (flash--add-label-overlay state pos end-pos label face position))))))))

(defun flash--add-label-overlay (state pos end-pos label face position)
  "Add label overlay to STATE at appropriate position.
POS is match start, END-POS is match end, LABEL is the label string,
FACE is the label face, POSITION is where to place the label."
  (let* ((prefix (flash-state-label-prefix state))
         ;; Show remaining part of label after prefix
         (display-label (if (and prefix (string-prefix-p prefix label))
                            (substring label (length prefix))
                          label))
         (label-str (propertize display-label 'face face))
         ov)
    (pcase position
      ('after
       ;; Label after match (default)
       (setq ov (make-overlay end-pos end-pos))
       (overlay-put ov 'after-string label-str))
      ('before
       ;; Label before match
       (setq ov (make-overlay pos pos))
       (overlay-put ov 'before-string label-str))
      ('overlay
       ;; Label replaces first character
       (setq ov (make-overlay pos (1+ pos)))
       (overlay-put ov 'display label-str))
      ('eol
       ;; Label at end of line
       (save-excursion
         (goto-char pos)
         (let ((eol (line-end-position)))
           (setq ov (make-overlay eol eol))
           (overlay-put ov 'after-string
                        (concat " " label-str)))))
      (_
       ;; Fallback to after
       (setq ov (make-overlay end-pos end-pos))
       (overlay-put ov 'after-string label-str)))
    (overlay-put ov 'flash t)
    (overlay-put ov 'priority 200)
    (push ov (flash-state-overlays state))))

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
