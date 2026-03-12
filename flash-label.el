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

;;; Configuration (defined via defcustom in flash.el)

(defvar flash-labels)
(defvar flash-label-uppercase)
(defvar flash-case-fold)

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
         ;; Deduplicate: keep only closest match per fold
         (sorted (let ((seen-folds (make-hash-table :test 'eql))
                       deduped)
                   (dolist (match sorted)
                     (let ((fold (flash-match-fold match)))
                       (if (and fold (gethash fold seen-folds))
                           nil  ; skip — another match in same fold is closer
                         (when fold (puthash fold t seen-folds))
                         (push match deduped))))
                   (nreverse deduped)))
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
Returns list of strings.  When COUNT <= (length CHARS), all labels are
single-char.  Otherwise, partition CHARS into single-char labels (from
the front) and multi-char prefixes (from the tail).  Multi-char labels
are generated breadth-first: depth 1 (prefix+single), depth 2
\(prefix+prefix+single), etc., so every match is always labeled.
Single-char labels never collide with multi-char prefixes."
  (let ((n (length chars)))
    (if (<= count n)
        ;; All single-char labels
        (mapcar #'char-to-string (cl-subseq chars 0 count))
      ;; Find minimum P (prefix count) such that (N-P)*(1+P) >= count.
      (let ((p 1))
        (while (and (< p n)
                    (< (* (- n p) (1+ p)) count))
          (cl-incf p))
        (when (>= p n)
          (setq p (max 1 (/ n 2))))
        (let* ((singles (cl-subseq chars 0 (- n p)))
               (prefixes (cl-subseq chars (- n p)))
               (labels (mapcar #'char-to-string singles))
               (multi nil)
               (remaining (- count (length labels)))
               ;; Prefix strings to extend at each depth level
               (cur-prefixes (mapcar #'char-to-string
                                     (append prefixes nil))))
          ;; Generate multi-char labels breadth-first by depth
          (catch 'done
            (while cur-prefixes
              (let ((next-prefixes nil))
                (dolist (pfx cur-prefixes)
                  ;; Terminal labels: prefix + single-char suffix
                  (dolist (s singles)
                    (push (concat pfx (char-to-string s)) multi)
                    (cl-decf remaining)
                    (when (<= remaining 0)
                      (throw 'done nil)))
                  ;; Extend prefix for next depth level
                  (dolist (pc prefixes)
                    (push (concat pfx (char-to-string pc))
                          next-prefixes)))
                (setq cur-prefixes (nreverse next-prefixes)))))
          ;; Singles first (closest matches), then multi (farthest)
          (append labels (nreverse multi)))))))

(defun flash--resolve-label-limit (available-chars max-matches)
  "Resolve assignment limit from AVAILABLE-CHARS and MAX-MATCHES.
Always uses full multi-char capacity (labels are generated automatically)."
  (let* ((single-capacity (length available-chars))
         (full-capacity (* single-capacity single-capacity)))
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
