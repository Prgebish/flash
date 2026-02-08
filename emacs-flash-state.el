;;; emacs-flash-state.el --- State management for emacs-flash -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: chestnykh
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;; Defines data structures for flash jump sessions.

;;; Code:

(require 'cl-lib)

;;; Data Structures

(cl-defstruct emacs-flash-state
  "State of a flash jump session."
  (pattern "")          ; current search pattern (string)
  (matches nil)         ; list of emacs-flash-match
  (windows nil)         ; windows to search in
  (overlays nil)        ; all created overlays
  (target nil)          ; current target match
  (start-window nil)    ; original window
  (start-point nil)     ; original point position
  (label-prefix nil)    ; current label prefix for multi-char labels
  (whole-buffer nil))   ; when t, check label conflicts in whole buffer (for search integration)

(cl-defstruct emacs-flash-match
  "A single search match."
  (pos nil)             ; start position (marker)
  (end-pos nil)         ; end position (marker)
  (label nil)           ; assigned label (string or nil, supports multi-char)
  (window nil)          ; window containing match
  (fold nil))           ; fold region start (or nil if not in fold)

;;; State Management

(defun emacs-flash-state-create (&optional windows)
  "Create new flash state.
WINDOWS is a list of windows to search in.
If nil, uses current window only."
  (make-emacs-flash-state
   :pattern ""
   :matches nil
   :windows (or windows (list (selected-window)))
   :overlays nil
   :target nil
   :start-window (selected-window)
   :start-point (point)))

(defun emacs-flash-state-cleanup (state)
  "Clean up STATE, remove all overlays."
  (mapc #'delete-overlay (emacs-flash-state-overlays state)))

(provide 'emacs-flash-state)
;;; emacs-flash-state.el ends here
