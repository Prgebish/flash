;;; emacs-flash-jump.el --- Jump logic for emacs-flash -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: chestnykh
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;; Jump to match position, handle window switching and fold unfolding.

;;; Code:

(require 'emacs-flash-state)
(require 'emacs-flash-label)

;;; Configuration (set by emacs-flash.el)

(defvar emacs-flash-jump-position)
(defvar emacs-flash-jumplist)
(defvar emacs-flash-nohlsearch)

;;; Forward declarations for evil
(defvar evil-ex-search-highlight-all)
(declare-function evil-ex-nohighlight "evil-ex")

;;; Jump Functions

(defun emacs-flash-jump-to-match (match)
  "Jump to MATCH position.
Switches window if needed, unfolds if target is in fold.
Uses `emacs-flash-jump-position' to determine cursor placement.
Saves to jumplist if `emacs-flash-jumplist' is non-nil.
Clears highlighting if `emacs-flash-nohlsearch' is non-nil."
  (when match
    (let ((win (emacs-flash-match-window match))
          (pos (emacs-flash-match-pos match))
          (end-pos (emacs-flash-match-end-pos match))
          (fold (emacs-flash-match-fold match))
          (jump-pos emacs-flash-jump-position))
      ;; Save to jumplist before jumping
      (when emacs-flash-jumplist
        (push-mark nil t))
      ;; Switch window if needed
      (unless (eq win (selected-window))
        (select-window win))
      ;; Jump to position based on setting
      (goto-char (marker-position
                  (if (eq jump-pos 'end) end-pos pos)))
      ;; Unfold if in fold
      (when fold
        (emacs-flash--unfold-at-point))
      ;; Clear search highlighting if requested
      (when emacs-flash-nohlsearch
        (emacs-flash--clear-search-highlight))
      t)))

(defun emacs-flash--clear-search-highlight ()
  "Clear search highlighting from isearch and evil-search."
  ;; Clear isearch highlight
  (when (bound-and-true-p isearch-mode)
    (isearch-done))
  (lazy-highlight-cleanup t)
  ;; Clear evil-search highlight
  (when (and (featurep 'evil)
             (fboundp 'evil-ex-nohighlight))
    (evil-ex-nohighlight)))

(defun emacs-flash-jump-to-label (state label-str)
  "Jump to match with label LABEL-STR in STATE.
Returns t if jump successful, nil otherwise."
  (when-let ((match (emacs-flash-find-match-by-label state label-str)))
    (emacs-flash-jump-to-match match)))

(defun emacs-flash-jump-to-first (state)
  "Jump to first match in STATE.
Returns t if jump successful, nil otherwise."
  (when-let ((match (car (emacs-flash-state-matches state))))
    (emacs-flash-jump-to-match match)))

(defun emacs-flash-return-to-start (state)
  "Return to start position saved in STATE."
  (let ((win (emacs-flash-state-start-window state))
        (pos (emacs-flash-state-start-point state)))
    (when (and win (window-live-p win))
      (select-window win))
    (when pos
      (goto-char pos))))

;;; Fold Handling

(defun emacs-flash--unfold-at-point ()
  "Unfold region at point if folded.
Supports outline-mode, org-mode, and hideshow."
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

(provide 'emacs-flash-jump)
;;; emacs-flash-jump.el ends here
