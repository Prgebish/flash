;;; flash-label.el --- Label assignment for flash -*- lexical-binding: t -*-

;; Copyright (C) 2025 Vadim Pavlov
;; Author: Vadim Pavlov <https://github.com/Prgebish>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Smart label assignment with conflict detection.
;; Labels that could continue the search pattern are skipped.

;;; Code:

(require 'cl-lib)
(require 'flash-state)

;;; Configuration (set by flash.el)

(defvar flash-labels)
(defvar flash-label-uppercase)
(defvar flash-multi-char-labels)
(defvar flash-case-fold)

;;; Label Functions

(defun flash-label-matches (state)
  "Assign labels to matches in STATE.
Labels are assigned to matches sorted by distance from cursor.
Labels that conflict with pattern continuation are skipped.
Uses multi-char labels when matches exceed available single-char labels."
  (let* ((matches (flash-state-matches state))
         (pattern (flash-state-pattern state))
         (available-chars (flash--available-labels state pattern))
         (sorted (flash--sort-by-distance state matches))
         (labels (flash--generate-labels available-chars (length sorted))))
    ;; Reset all labels first
    (dolist (match matches)
      (setf (flash-match-label match) nil))
    ;; Assign labels to sorted matches
    (cl-loop for match in sorted
             for label in labels
             do (setf (flash-match-label match) label))))

(defun flash--generate-labels (chars count)
  "Generate COUNT labels from CHARS.  Return them as strings.
Returns list of strings.  Uses single chars when possible.
When `flash-multi-char-labels' is non-nil and COUNT > (length CHARS),
generates multi-char labels (aa, as, ad, ...).
When `flash-multi-char-labels' is nil, excess matches remain unlabeled."
  (let ((n (length chars)))
    (if (<= count n)
        ;; Single char labels
        (mapcar #'char-to-string (cl-subseq chars 0 count))
      ;; More matches than single chars
      (if flash-multi-char-labels
          ;; Multi-char labels enabled
          (let ((labels nil)
                (needed count))
            ;; Generate two-char combinations
            (catch 'done
              (dolist (c1 chars)
                (dolist (c2 chars)
                  (push (string c1 c2) labels)
                  (cl-decf needed)
                  (when (<= needed 0)
                    (throw 'done nil)))))
            (nreverse labels))
        ;; Multi-char disabled - only use available single chars
        (mapcar #'char-to-string chars)))))

(defun flash--continuation-chars (state)
  "Collect characters that could continue the current search pattern.
Returns a hash table of characters.  When `flash-case-fold' is non-nil,
stores downcased characters.
Reads char-after end-pos of each existing match."
  (let ((chars (make-hash-table :test 'eq)))
    (dolist (match (flash-state-matches state))
      (let ((end (flash-match-end-pos match)))
        (when (and (markerp end) (marker-buffer end))
          (with-current-buffer (marker-buffer end)
            (when (< (marker-position end) (point-max))
              (let ((c (char-after end)))
                (when c
                  (puthash (if flash-case-fold (downcase c) c)
                           t chars))))))))
    chars))

(defun flash--available-labels (state pattern)
  "Return labels that won't conflict with PATTERN continuation.
STATE is used to collect continuation characters from matches.
When `flash-label-uppercase' is non-nil, includes uppercase versions."
  (let* ((base-chars (string-to-list flash-labels))
         (chars (if flash-label-uppercase
                    ;; Add uppercase versions of alphabetic chars
                    (append base-chars
                            (cl-remove-if-not
                             #'identity
                             (mapcar (lambda (c)
                                       (let ((up (upcase c)))
                                         (unless (= c up) up)))
                                     base-chars)))
                  base-chars)))
    (if (string-empty-p pattern)
        chars
      ;; Build continuation hash once, filter with O(1) lookups
      (let ((cont-chars (flash--continuation-chars state)))
        (cl-remove-if
         (lambda (char)
           ;; Uppercase labels never conflict when uppercase mode is enabled
           (unless (and flash-label-uppercase
                        (>= char ?A) (<= char ?Z))
             (gethash (if flash-case-fold (downcase char) char)
                      cont-chars)))
         chars)))))

(defun flash--sort-by-distance (state matches)
  "Sort MATCHES by distance from cursor position.
STATE provides the reference position via start-point.
Matches at cursor position are sorted last (for continue functionality)."
  (let ((pos (or (flash-state-start-point state) (point))))
    (sort (copy-sequence matches)
          (lambda (a b)
            (let ((dist-a (abs (- (marker-position (flash-match-pos a)) pos)))
                  (dist-b (abs (- (marker-position (flash-match-pos b)) pos))))
              ;; Matches at cursor (distance 0) go last
              (cond
               ((and (= dist-a 0) (= dist-b 0)) nil)
               ((= dist-a 0) nil)  ; a goes after b
               ((= dist-b 0) t)    ; a goes before b
               (t (< dist-a dist-b))))))))

(defun flash-find-match-by-label (state label-str)
  "Find match with exact label LABEL-STR in STATE."
  (cl-find label-str (flash-state-matches state)
           :key #'flash-match-label
           :test #'equal))

(defun flash-matches-with-label-prefix (state prefix)
  "Return matches in STATE whose labels start with PREFIX."
  (cl-remove-if-not
   (lambda (match)
     (let ((label (flash-match-label match)))
       (and label
            (stringp label)
            (string-prefix-p prefix label))))
   (flash-state-matches state)))

(provide 'flash-label)
;;; flash-label.el ends here
