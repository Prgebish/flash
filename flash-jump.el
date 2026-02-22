;;; flash-jump.el --- Jump logic for flash -*- lexical-binding: t -*-

;; Copyright (C) 2025 Vadim Pavlov
;; Author: Vadim Pavlov <https://github.com/Prgebish>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Jump to match position, handle window switching and fold unfolding.

;;; Code:

(require 'flash-state)
(require 'flash-label)

(declare-function flash-match-pos-value "flash-state" (match))
(declare-function flash-match-end-pos-value "flash-state" (match))
(declare-function flash-match-buffer-live "flash-state" (match))

;;; Configuration (set by flash.el)

(defvar flash-jump-position)
(defvar flash-jumplist)
(defvar flash-nohlsearch)

;;; Forward declarations for evil
(defvar evil-state)
(defvar evil-ex-search-highlight-all)
(declare-function evil-ex-nohighlight "evil-ex")

;;; Jump Functions

(defun flash-jump-to-match (match)
  "Jump to MATCH position.
Switches window if needed, unfolds if target is in fold.
Uses `flash-jump-position' to determine cursor placement.
Saves to jumplist if `flash-jumplist' is non-nil.
Clears highlighting if `flash-nohlsearch' is non-nil."
  (when match
    (let* ((win (flash-match-window match))
           (buf (flash-match-buffer-live match))
           (target-window (cond
                           ((window-live-p win) win)
                           ((and (buffer-live-p buf)
                                 (get-buffer-window buf t)))
                           (t (selected-window))))
          (pos (flash-match-pos-value match))
          (end-pos (flash-match-end-pos-value match))
          (fold (flash-match-fold match))
          (jump-pos flash-jump-position)
          (target-pos (if (eq jump-pos 'end) end-pos pos)))
      ;; Save to jumplist before jumping.
      ;; Skip when region is active: push-mark overwrites the selection
      ;; anchor (mark).  Covers both vanilla Emacs (transient-mark-mode)
      ;; and evil visual state.
      (when (and flash-jumplist
                 (not (and transient-mark-mode mark-active))
                 (not (and (bound-and-true-p evil-local-mode)
                           (eq evil-state 'visual))))
        (push-mark nil t))
      ;; Switch window if needed
      (unless (eq target-window (selected-window))
        (select-window target-window))
      ;; Ensure the target window shows the match buffer when possible.
      (when (and (buffer-live-p buf)
                 (not (eq (window-buffer target-window) buf))
                 (not (window-dedicated-p target-window)))
        (set-window-buffer target-window buf))
      ;; Jump to position based on setting.
      (when (and (integerp target-pos)
                 (<= (point-min) target-pos)
                 (<= target-pos (point-max)))
        (goto-char target-pos))
      ;; Unfold if in fold
      (when fold
        (flash--unfold-at-point))
      ;; Clear search highlighting if requested
      (when flash-nohlsearch
        (flash--clear-search-highlight))
      t)))

(defun flash--clear-search-highlight ()
  "Clear search highlighting from isearch and evil-search."
  ;; Clear isearch highlight
  (when (bound-and-true-p isearch-mode)
    (isearch-done))
  (lazy-highlight-cleanup t)
  ;; Clear evil-search highlight
  (when (and (featurep 'evil)
             (fboundp 'evil-ex-nohighlight))
    (evil-ex-nohighlight)))

(defun flash-jump-to-label (state label-str)
  "Jump to match with label LABEL-STR in STATE.
Returns t if jump successful, nil otherwise."
  (when-let ((match (flash-find-match-by-label state label-str)))
    (flash-jump-to-match match)))

(defun flash-jump-to-first (state)
  "Jump to first match in STATE.
Returns t if jump successful, nil otherwise."
  (when-let ((match (car (flash-state-matches state))))
    (flash-jump-to-match match)))

(defun flash-return-to-start (state)
  "Return to start position saved in STATE."
  (let ((win (flash-state-start-window state))
        (pos (flash-state-start-point state)))
    (when (and win (window-live-p win))
      (select-window win))
    (when pos
      (goto-char pos))))

;;; Fold Handling

(defun flash--unfold-at-point ()
  "Unfold region at point if folded.
Supports `outline-mode', `org-mode', and hideshow."
  (cond
   ;; Org-mode
   ((and (derived-mode-p 'org-mode)
         (fboundp 'org-show-context))
    (org-show-context 'link-search))
   ;; Outline-mode
   ((and (or (derived-mode-p 'outline-mode)
             (bound-and-true-p outline-minor-mode))
         (fboundp 'outline-show-entry))
    (outline-show-entry))
   ;; Hideshow
   ((and (bound-and-true-p hs-minor-mode)
         (fboundp 'hs-show-block))
    (hs-show-block))
   ;; Generic: try to make point visible
   (t
    (when (invisible-p (point))
      (let ((inhibit-read-only t))
        (put-text-property (point) (1+ (point)) 'invisible nil))))))

(provide 'flash-jump)
;;; flash-jump.el ends here
