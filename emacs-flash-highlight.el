;;; emacs-flash-highlight.el --- Highlighting for emacs-flash -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: chestnykh
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;; Visual feedback using overlays: backdrop, match highlighting, labels.

;;; Code:

(require 'emacs-flash-state)

;;; Faces
;; Default faces inherit from theme, rainbow uses hardcoded Tailwind colors

(defface emacs-flash-label
  '((((background light))
     :background "#3b82f6" :foreground "#ffffff" :weight bold)
    (((background dark))
     :background "#fecdd3" :foreground "#881337" :weight bold))
  "Face for jump labels.
Blue on light, light pink on dark (Tailwind blue-500, rose-200/900).
Customize with `M-x customize-face RET emacs-flash-label' or `custom-set-faces'."
  :group 'emacs-flash)

(defface emacs-flash-match
  '((t :underline t))
  "Face for search matches.
Subtle underline (uses foreground color) to mark matched text."
  :group 'emacs-flash)

(defface emacs-flash-backdrop
  '((t :inherit shadow))
  "Face for backdrop effect.
Inherits from `shadow' (like flash.nvim's FlashBackdrop → Comment)."
  :group 'emacs-flash)

;;; Rainbow label faces
;; Hardcoded Tailwind CSS colors (shade 200/900), like flash.nvim with shade=2

(defface emacs-flash-label-red
  '((t :background "#fecaca" :foreground "#7f1d1d" :weight bold))
  "Rainbow label - red (Tailwind red-200/900)."
  :group 'emacs-flash)

(defface emacs-flash-label-amber
  '((t :background "#fde68a" :foreground "#78350f" :weight bold))
  "Rainbow label - amber (Tailwind amber-200/900)."
  :group 'emacs-flash)

(defface emacs-flash-label-lime
  '((t :background "#d9f99d" :foreground "#365314" :weight bold))
  "Rainbow label - lime (Tailwind lime-200/900)."
  :group 'emacs-flash)

(defface emacs-flash-label-green
  '((t :background "#bbf7d0" :foreground "#14532d" :weight bold))
  "Rainbow label - green (Tailwind green-200/900)."
  :group 'emacs-flash)

(defface emacs-flash-label-teal
  '((t :background "#99f6e4" :foreground "#134e4a" :weight bold))
  "Rainbow label - teal (Tailwind teal-200/900)."
  :group 'emacs-flash)

(defface emacs-flash-label-cyan
  '((t :background "#a5f3fc" :foreground "#164e63" :weight bold))
  "Rainbow label - cyan (Tailwind cyan-200/900)."
  :group 'emacs-flash)

(defface emacs-flash-label-blue
  '((t :background "#bfdbfe" :foreground "#1e3a8a" :weight bold))
  "Rainbow label - blue (Tailwind blue-200/900)."
  :group 'emacs-flash)

(defface emacs-flash-label-violet
  '((t :background "#ddd6fe" :foreground "#4c1d95" :weight bold))
  "Rainbow label - violet (Tailwind violet-200/900)."
  :group 'emacs-flash)

(defface emacs-flash-label-fuchsia
  '((t :background "#f5d0fe" :foreground "#701a75" :weight bold))
  "Rainbow label - fuchsia (Tailwind fuchsia-200/900)."
  :group 'emacs-flash)

(defface emacs-flash-label-rose
  '((t :background "#fecdd3" :foreground "#881337" :weight bold))
  "Rainbow label - rose (Tailwind rose-200/900)."
  :group 'emacs-flash)

(defvar emacs-flash-rainbow-faces
  '(emacs-flash-label-red
    emacs-flash-label-amber
    emacs-flash-label-lime
    emacs-flash-label-green
    emacs-flash-label-teal
    emacs-flash-label-cyan
    emacs-flash-label-blue
    emacs-flash-label-violet
    emacs-flash-label-fuchsia
    emacs-flash-label-rose)
  "List of faces to cycle through for rainbow labels.
Same 10 colors as flash.nvim: red, amber, lime, green, teal, cyan, blue, violet, fuchsia, rose.")

;;; Configuration (set by emacs-flash.el)

(defvar emacs-flash-backdrop)
(defvar emacs-flash-rainbow)
(defvar emacs-flash-highlight-matches)
(defvar emacs-flash-label-position)

;;; Highlight Functions

(defun emacs-flash-highlight-update (state)
  "Update highlighting for STATE.
Clears old overlays and creates new ones for backdrop, matches, and labels."
  ;; Remove old overlays
  (emacs-flash-highlight-clear state)

  ;; Backdrop
  (when emacs-flash-backdrop
    (emacs-flash--highlight-backdrop state))

  ;; Matches and labels
  (let ((index 0))
    (dolist (match (emacs-flash-state-matches state))
      (emacs-flash--highlight-match state match index)
      (when (emacs-flash-match-label match)
        (setq index (1+ index))))))

(defun emacs-flash-highlight-clear (state)
  "Remove all overlays from STATE."
  (mapc #'delete-overlay (emacs-flash-state-overlays state))
  (setf (emacs-flash-state-overlays state) nil))

(defun emacs-flash--highlight-backdrop (state)
  "Add backdrop overlay to all windows in STATE."
  (dolist (win (emacs-flash-state-windows state))
    (when (window-live-p win)
      (with-current-buffer (window-buffer win)
        (let ((ov (make-overlay (window-start win) (window-end win t))))
          (overlay-put ov 'face 'emacs-flash-backdrop)
          (overlay-put ov 'emacs-flash t)
          (overlay-put ov 'priority 0)
          (push ov (emacs-flash-state-overlays state)))))))

(defun emacs-flash--highlight-match (state match index)
  "Add overlays for MATCH to STATE.
INDEX is used to select rainbow color when `emacs-flash-rainbow' is enabled."
  (let* ((pos (emacs-flash-match-pos match))
         (end-pos (emacs-flash-match-end-pos match))
         (label (emacs-flash-match-label match))
         (prefix (emacs-flash-state-label-prefix state))
         (fold (emacs-flash-match-fold match))
         ;; Skip labels that don't match current prefix
         (show-label (and label
                          (or (not prefix)
                              (string-prefix-p prefix label))))
         (face (when show-label (emacs-flash--get-label-face index)))
         (buf (marker-buffer pos))
         (position emacs-flash-label-position))
    (when buf
      (with-current-buffer buf
        (cond
         ;; Folded: no highlighting (match is invisible)
         (fold nil)
         ;; Normal: underline match and show label
         (t
          (when emacs-flash-highlight-matches
            (let ((ov (make-overlay pos end-pos)))
              (overlay-put ov 'face 'emacs-flash-match)
              (overlay-put ov 'emacs-flash t)
              (overlay-put ov 'priority 100)
              (push ov (emacs-flash-state-overlays state))))
          (when show-label
            (emacs-flash--add-label-overlay state pos end-pos label face position))))))))

(defun emacs-flash--add-label-overlay (state pos end-pos label face position)
  "Add label overlay to STATE at appropriate position.
POS is match start, END-POS is match end, LABEL is the label string,
FACE is the label face, POSITION is where to place the label."
  (let* ((prefix (emacs-flash-state-label-prefix state))
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
    (overlay-put ov 'emacs-flash t)
    (overlay-put ov 'priority 200)
    (push ov (emacs-flash-state-overlays state))))

(defun emacs-flash--get-label-face (index)
  "Get face for label at INDEX.
Returns rainbow face if `emacs-flash-rainbow' is enabled, otherwise default."
  (if (and (bound-and-true-p emacs-flash-rainbow)
           emacs-flash-rainbow-faces)
      (nth (mod index (length emacs-flash-rainbow-faces))
           emacs-flash-rainbow-faces)
    'emacs-flash-label))

(provide 'emacs-flash-highlight)
;;; emacs-flash-highlight.el ends here
