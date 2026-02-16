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
Returns list of strings.  When COUNT <= (length CHARS), all labels are
single-char.  Otherwise, partition CHARS: reserve the last P chars as
multi-char prefixes, use the remaining N-P as single-char labels.
Multi-char labels are prefix + any non-prefix char, giving P*(N-P)
combinations.  Total capacity: (N-P)*(1+P).  Single-char labels never
collide with multi-char prefixes, so exact-match dispatch works."
  (let ((n (length chars)))
    (if (<= count n)
        ;; All single-char labels
        (mapcar #'char-to-string (cl-subseq chars 0 count))
      ;; Find minimum P (prefix count) such that (N-P)*(1+P) >= count
      (let ((p 1))
        (while (and (< p n)
                    (< (* (- n p) (1+ p)) count))
          (cl-incf p))
        ;; Clamp: need at least 1 single-char slot
        (when (>= p n) (setq p (1- n)))
        (let* ((singles (cl-subseq chars 0 (- n p)))
               (prefixes (cl-subseq chars (- n p)))
               (labels (mapcar #'char-to-string singles))
               (multi nil)
               (needed (- count (length labels))))
          ;; Generate multi-char labels: each prefix paired with each single
          (catch 'done
            (dolist (pre prefixes)
              (dolist (suf singles)
                (push (string pre suf) multi)
                (cl-decf needed)
                (when (<= needed 0)
                  (throw 'done nil)))))
          ;; Singles first (closest matches), then multi (farthest)
          (append labels (nreverse multi)))))))

(defun flash--available-labels (state pattern)
  "Return labels that won't conflict with PATTERN continuation.
STATE is used to check for conflicts in all search windows.
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
      ;; Skip labels that could continue the pattern
      (cl-remove-if
       (lambda (char)
         (flash--label-conflicts-p state pattern char))
       chars))))

(defun flash--label-conflicts-p (state pattern char)
  "Return non-nil if CHAR would continue PATTERN to a real match.
STATE provides the windows to search in and search scope.
When `flash-state-whole-buffer' is non-nil, searches entire buffers
\(for search integration where matches can be anywhere).
Otherwise only searches visible portions of windows.
When `flash-label-uppercase' is enabled, uppercase CHAR never conflicts
because user input is distinguished by case: lowercase continues search,
uppercase selects label."
  ;; Uppercase labels don't conflict when uppercase mode is enabled
  (when (or (not flash-label-uppercase)
            (not (and (>= char ?A) (<= char ?Z))))
    (let ((extended (concat pattern (char-to-string char)))
          (whole-buffer (flash-state-whole-buffer state)))
      (cl-some
       (lambda (win)
         (when (window-live-p win)
           (with-current-buffer (window-buffer win)
             (save-excursion
               (goto-char (if whole-buffer (point-min) (window-start win)))
               (let ((case-fold-search flash-case-fold))
                 (search-forward extended
                                 (unless whole-buffer (window-end win t))
                                 t))))))
       (flash-state-windows state)))))

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
