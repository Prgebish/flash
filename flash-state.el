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
  (label-index nil)     ; hash table: label string -> match
  (windows nil)         ; windows to search in
  (overlays nil)        ; all created overlays
  (target nil)          ; current target match
  (start-window nil)    ; original window
  (start-point nil)     ; original point position
  (label-prefix nil)    ; current label prefix for multi-char labels
  (backdrop-overlays nil)) ; backdrop overlays (reused across updates)

(cl-defstruct flash-match
  "A single search match."
  (pos nil)             ; start position (integer or marker)
  (end-pos nil)         ; end position (integer or marker)
  (buffer nil)          ; buffer containing the match
  (label nil)           ; assigned label (string or nil, supports multi-char)
  (window nil)          ; window containing match
  (fold nil))           ; fold region start (or nil if not in fold)

(defun flash-match-pos-value (match)
  "Return numeric start position for MATCH, or nil."
  (let ((pos (flash-match-pos match)))
    (cond
     ((integerp pos) pos)
     ((markerp pos) (marker-position pos))
     (t nil))))

(defun flash-match-end-pos-value (match)
  "Return numeric end position for MATCH, or nil."
  (let ((end-pos (flash-match-end-pos match)))
    (cond
     ((integerp end-pos) end-pos)
     ((markerp end-pos) (marker-position end-pos))
     (t nil))))

(defun flash-match-buffer-live (match)
  "Return live buffer for MATCH, or nil."
  (or (and (markerp (flash-match-pos match))
           (buffer-live-p (marker-buffer (flash-match-pos match)))
           (marker-buffer (flash-match-pos match)))
      (and (markerp (flash-match-end-pos match))
           (buffer-live-p (marker-buffer (flash-match-end-pos match)))
           (marker-buffer (flash-match-end-pos match)))
      (and (buffer-live-p (flash-match-buffer match))
           (flash-match-buffer match))
      (and (window-live-p (flash-match-window match))
           (buffer-live-p (window-buffer (flash-match-window match)))
           (window-buffer (flash-match-window match)))))

(defun flash-match-release-markers (match)
  "Release marker objects held by MATCH."
  (when (markerp (flash-match-pos match))
    (set-marker (flash-match-pos match) nil))
  (when (markerp (flash-match-end-pos match))
    (set-marker (flash-match-end-pos match) nil)))

;;; State Management

(defun flash-state-create (&optional windows)
  "Create new flash state.
WINDOWS is a list of windows to search in.
If nil, uses current window only."
  (make-flash-state
   :pattern ""
   :matches nil
   :label-index nil
   :windows (or windows (list (selected-window)))
   :overlays nil
   :target nil
   :start-window (selected-window)
   :start-point (point)))

(defun flash-state-cleanup (state)
  "Clean up STATE: delete overlays (including backdrop) and release markers."
  (mapc #'delete-overlay (flash-state-overlays state))
  (setf (flash-state-overlays state) nil)
  (mapc #'delete-overlay (flash-state-backdrop-overlays state))
  (setf (flash-state-backdrop-overlays state) nil)
  (dolist (m (flash-state-matches state))
    (flash-match-release-markers m))
  (setf (flash-state-matches state) nil)
  (setf (flash-state-label-index state) nil))

(provide 'flash-state)
;;; flash-state.el ends here
