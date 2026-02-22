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

(declare-function flash-match-pos-value "flash-state" (match))
(declare-function flash-match-end-pos-value "flash-state" (match))
(declare-function flash-match-buffer-live "flash-state" (match))

;;; Configuration (set by flash.el)

(defvar flash-labels "asdfjkl;ghqwertyuiopzxcvbnm")
(defvar flash-label-uppercase nil)
(defvar flash-multi-char-labels nil)
(defvar flash-case-fold t)

;;; Label Functions

(defun flash-label-matches (state &optional max-matches)
  "Assign labels to matches in STATE and return labeled candidates.
Labels are assigned by distance from cursor.
Labels that conflict with pattern continuation are skipped.
MAX-MATCHES controls assignment limit:
- nil: use full label capacity (single-char or two-char).
- `single-char': limit to one-char label capacity.
- integer: assign at most that many matches."
  (let* ((matches (flash-state-matches state))
         (pattern (flash-state-pattern state))
         (available-chars (flash--available-labels state pattern))
         (limit (flash--resolve-label-limit available-chars max-matches))
         (sorted (flash--sort-by-distance state matches limit))
         (labels (flash--generate-labels available-chars (length sorted)))
         (label-index (make-hash-table :test 'equal)))
    ;; Reset all labels first.
    (dolist (match matches)
      (setf (flash-match-label match) nil))
    (setf (flash-state-label-index state) nil)
    ;; Assign labels to the top candidates.
    (cl-loop for match in sorted
             for label in labels
             do (when label
                  (setf (flash-match-label match) label)
                  (puthash label match label-index)))
    (setf (flash-state-label-index state) label-index)
    sorted))

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

(defun flash--resolve-label-limit (available-chars max-matches)
  "Resolve assignment limit from AVAILABLE-CHARS and MAX-MATCHES."
  (let* ((single-capacity (length available-chars))
         (full-capacity (if flash-multi-char-labels
                            (* single-capacity single-capacity)
                          single-capacity)))
    (cond
     ((eq max-matches 'single-char) single-capacity)
     ((and (integerp max-matches) (>= max-matches 0))
      (min max-matches full-capacity))
     (t full-capacity))))

(defun flash--continuation-chars (state)
  "Collect characters that could continue the current search pattern.
Returns a hash table of characters.  When `flash-case-fold' is non-nil,
stores downcased characters.
Reads char-after end-pos of each existing match."
  (let ((chars (make-hash-table :test 'eq)))
    (dolist (match (flash-state-matches state))
      (let ((end (flash-match-end-pos-value match))
            (buffer (flash-match-buffer-live match)))
        (when (and (integerp end)
                   (buffer-live-p buffer))
          (with-current-buffer buffer
            (when (< end (point-max))
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

(defun flash--distance-score (origin-pos match)
  "Return sortable distance score from ORIGIN-POS to MATCH.
Exact cursor matches get the largest score so they sort last."
  (let* ((match-pos (or (flash-match-pos-value match) origin-pos))
         (dist (abs (- match-pos origin-pos))))
    (if (= dist 0) most-positive-fixnum dist)))

(defun flash--heap-swap (heap i j)
  "Swap entries at I and J in HEAP."
  (let ((tmp (aref heap i)))
    (aset heap i (aref heap j))
    (aset heap j tmp)))

(defun flash--heap-sift-up (heap index)
  "Move HEAP entry at INDEX upward in max-heap order."
  (while (> index 1)
    (let* ((parent (/ index 2))
           (entry (aref heap index))
           (parent-entry (aref heap parent)))
      (if (> (car entry) (car parent-entry))
          (progn
            (flash--heap-swap heap index parent)
            (setq index parent))
        (setq index 1)))))

(defun flash--heap-sift-down (heap size index)
  "Move HEAP entry at INDEX downward in max-heap of SIZE."
  (let ((keep-running t))
    (while keep-running
      (let* ((left (* 2 index))
             (right (1+ left))
             (largest index))
        (when (and (<= left size)
                   (> (car (aref heap left))
                      (car (aref heap largest))))
          (setq largest left))
        (when (and (<= right size)
                   (> (car (aref heap right))
                      (car (aref heap largest))))
          (setq largest right))
        (if (= largest index)
            (setq keep-running nil)
          (flash--heap-swap heap index largest)
          (setq index largest))))))

(defun flash--top-k-by-distance (origin-pos matches limit)
  "Return LIMIT nearest MATCHES to ORIGIN-POS, sorted by distance."
  (let ((heap (make-vector (1+ limit) nil))
        (size 0))
    (dolist (match matches)
      (let ((entry (cons (flash--distance-score origin-pos match) match)))
        (if (< size limit)
            (progn
              (setq size (1+ size))
              (aset heap size entry)
              (flash--heap-sift-up heap size))
          (when (< (car entry) (car (aref heap 1)))
            (aset heap 1 entry)
            (flash--heap-sift-down heap size 1)))))
    (let (top-k)
      (dotimes (i size)
        (push (aref heap (1+ i)) top-k))
      (mapcar #'cdr
              (sort top-k
                    (lambda (a b) (< (car a) (car b))))))))

(defun flash--sort-by-distance (state matches &optional limit)
  "Sort MATCHES by distance from cursor position.
STATE provides the reference position via start-point.
Matches at cursor position are sorted last (for continue functionality).
When LIMIT is non-nil, return only nearest LIMIT matches."
  (let* ((origin-pos (or (flash-state-start-point state) (point)))
         (limit (if (and (integerp limit) (>= limit 0)) limit nil))
         (count (length matches)))
    (cond
     ((null matches) nil)
     ((and limit (= limit 0)) nil)
     ((and limit (< limit count))
      (flash--top-k-by-distance origin-pos matches limit))
     (t
      (mapcar #'cdr
              (sort (mapcar (lambda (match)
                              (cons (flash--distance-score origin-pos match)
                                    match))
                            matches)
                    (lambda (a b) (< (car a) (car b)))))))))

(defun flash-find-match-by-label (state label-str)
  "Find match with exact label LABEL-STR in STATE."
  (let ((index (flash-state-label-index state)))
    (if (hash-table-p index)
        (gethash label-str index)
      (cl-find label-str (flash-state-matches state)
               :key #'flash-match-label
               :test #'equal))))

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
