;;; flash-state.el --- State management for flash -*- lexical-binding: t -*-

;; Copyright (C) 2025 Vadim Pavlov
;; Author: Vadim Pavlov <https://github.com/Prgebish>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Defines data structures for flash jump sessions.

;;; Code:

(require 'cl-lib)

;;; Data Structures

(cl-defstruct flash-state
  "State of a flash jump session."
  (pattern "")          ; current search pattern (string)
  (matches nil)         ; list of flash-match
  (windows nil)         ; windows to search in
  (overlays nil)        ; all created overlays
  (target nil)          ; current target match
  (start-window nil)    ; original window
  (start-point nil)     ; original point position
  (label-prefix nil)    ; current label prefix for multi-char labels
  (whole-buffer nil))   ; when t, check label conflicts in whole buffer (for search integration)

(cl-defstruct flash-match
  "A single search match."
  (pos nil)             ; start position (marker)
  (end-pos nil)         ; end position (marker)
  (label nil)           ; assigned label (string or nil, supports multi-char)
  (window nil)          ; window containing match
  (fold nil))           ; fold region start (or nil if not in fold)

;;; State Management

(defun flash-state-create (&optional windows)
  "Create new flash state.
WINDOWS is a list of windows to search in.
If nil, uses current window only."
  (make-flash-state
   :pattern ""
   :matches nil
   :windows (or windows (list (selected-window)))
   :overlays nil
   :target nil
   :start-window (selected-window)
   :start-point (point)))

(defun flash-state-cleanup (state)
  "Clean up STATE: delete overlays and release markers."
  (mapc #'delete-overlay (flash-state-overlays state))
  (setf (flash-state-overlays state) nil)
  (dolist (m (flash-state-matches state))
    (when (markerp (flash-match-pos m))
      (set-marker (flash-match-pos m) nil))
    (when (markerp (flash-match-end-pos m))
      (set-marker (flash-match-end-pos m) nil))))

(provide 'flash-state)
;;; flash-state.el ends here
