;;; flash-label-test.el --- Tests for flash-label -*- lexical-binding: t -*-

;;; Commentary:
;; ERT tests for flash-label module.

;;; Code:

(require 'ert)
(require 'flash-state)
(require 'flash-label)

(ert-deftest flash-label-assigns-labels-test ()
  "Test that labels are assigned to matches."
  (with-temp-buffer
    (insert "foo bar foo")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window)))))
      (setf (flash-state-pattern state) "foo")
      ;; Add matches manually
      (setf (flash-state-matches state)
            (list (make-flash-match
                   :pos (copy-marker 1)
                   :end-pos (copy-marker 4)
                   :label nil
                   :window (selected-window)
                   :fold nil)
                  (make-flash-match
                   :pos (copy-marker 9)
                   :end-pos (copy-marker 12)
                   :label nil
                   :window (selected-window)
                   :fold nil)))
      (flash-label-matches state)
      ;; Both should have labels
      (should (flash-match-label (car (flash-state-matches state))))
      (should (flash-match-label (cadr (flash-state-matches state))))
      ;; Labels should be different
      (should-not (eq (flash-match-label (car (flash-state-matches state)))
                      (flash-match-label (cadr (flash-state-matches state))))))))

(ert-deftest flash-label-sorted-by-distance-test ()
  "Test that closer matches get earlier labels."
  (with-temp-buffer
    (insert "foo bar foo baz foo")
    (goto-char 10)  ; Position cursor in middle
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window)))))
      (setf (flash-state-pattern state) "foo")
      (setf (flash-state-start-point state) 10)
      ;; Add matches at positions 1, 9, 17
      (setf (flash-state-matches state)
            (list (make-flash-match
                   :pos (copy-marker 1)
                   :end-pos (copy-marker 4)
                   :label nil
                   :window (selected-window)
                   :fold nil)
                  (make-flash-match
                   :pos (copy-marker 9)
                   :end-pos (copy-marker 12)
                   :label nil
                   :window (selected-window)
                   :fold nil)
                  (make-flash-match
                   :pos (copy-marker 17)
                   :end-pos (copy-marker 20)
                   :label nil
                   :window (selected-window)
                   :fold nil)))
      (flash-label-matches state)
      ;; Match at pos 9 (closest to 10) should have first label "a"
      (let ((match-at-9 (cl-find 9 (flash-state-matches state)
                                 :key (lambda (m) (marker-position (flash-match-pos m))))))
        (should (equal "a" (flash-match-label match-at-9)))))))

(ert-deftest flash-label-conflict-detection-test ()
  "Test that conflicting labels are skipped."
  (with-temp-buffer
    (insert "ab abc abd")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window)))))
      (setf (flash-state-pattern state) "ab")
      ;; 'c' and 'd' would conflict (abc, abd exist)
      (let ((labels (flash--available-labels state "ab")))
        ;; 'c' and 'd' should not be in available labels
        (should-not (memq ?c labels))
        (should-not (memq ?d labels))
        ;; 'a' should be available (aba doesn't exist)
        (should (memq ?a labels))))))

(ert-deftest flash-label-empty-pattern-test ()
  "Test that all labels available for empty pattern."
  (with-temp-buffer
    (insert "test")
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window)))))
      (let ((labels (flash--available-labels state "")))
        ;; All labels should be available (base + uppercase if enabled)
        (should (>= (length labels) (length flash-labels)))))))

(ert-deftest flash-find-match-by-label-test ()
  "Test finding match by label."
  (with-temp-buffer
    (insert "foo bar")
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window)))))
      (setf (flash-state-matches state)
            (list (make-flash-match
                   :pos (copy-marker 1)
                   :end-pos (copy-marker 4)
                   :label "x"
                   :window (selected-window)
                   :fold nil)
                  (make-flash-match
                   :pos (copy-marker 5)
                   :end-pos (copy-marker 8)
                   :label "y"
                   :window (selected-window)
                   :fold nil)))
      ;; Find match with label 'x'
      (let ((match (flash-find-match-by-label state "x")))
        (should match)
        (should (= 1 (marker-position (flash-match-pos match)))))
      ;; Find match with label 'y'
      (let ((match (flash-find-match-by-label state "y")))
        (should match)
        (should (= 5 (marker-position (flash-match-pos match)))))
      ;; Non-existent label returns nil
      (should-not (flash-find-match-by-label state "z")))))

(ert-deftest flash-label-no-matches-test ()
  "Test labeling with no matches."
  (with-temp-buffer
    (insert "test")
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window)))))
      (setf (flash-state-matches state) nil)
      ;; Should not error
      (flash-label-matches state)
      (should (null (flash-state-matches state))))))

(ert-deftest flash-label-mixed-labeling-test ()
  "Test automatic mixed labeling: single-char for close, multi-char for far.
With 2 label chars (a,b) and 5 matches: P=1 prefix needed.
Singles: first 1 char (a) = 1 label.  Multi: prefix b * singles (a) = 1.
Capacity (N-P)*(1+P) = 1*2 = 2, not enough for 5.
With 3 chars (a,b,c): P=1, singles=2 (a,b), prefix=c, capacity=2*2=4, not enough.
P=2, singles=1 (a), prefixes=(b,c), capacity=1*3=3, not enough.
Use 4 chars (a,b,c,d): P=1, singles=3, prefix=d, capacity=3*2=6 >= 5."
  (with-temp-buffer
    (insert (make-string 100 ?x))
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let* ((flash-labels "abcd")
           (flash-label-uppercase nil)
           (state (flash-state-create (list (selected-window)))))
      ;; Create 5 matches at increasing distances from point-min
      (setf (flash-state-start-point state) 1)
      (setf (flash-state-matches state)
            (cl-loop for i from 1 to 5
                     collect (make-flash-match
                              :pos (copy-marker (* i 10))
                              :end-pos (copy-marker (1+ (* i 10)))
                              :label nil
                              :window (selected-window)
                              :fold nil)))
      (flash-label-matches state)
      ;; All 5 matches should be labeled
      (let ((labeled (cl-count-if #'flash-match-label
                                  (flash-state-matches state))))
        (should (= 5 labeled)))
      ;; Closest match should have single-char label
      ;; (match at pos 10, closest to start-point 1)
      (let* ((sorted-by-dist (sort (copy-sequence (flash-state-matches state))
                                   (lambda (a b)
                                     (< (marker-position (flash-match-pos a))
                                        (marker-position (flash-match-pos b))))))
             (closest (car sorted-by-dist)))
        (should (= 1 (length (flash-match-label closest)))))
      ;; Farthest match should have multi-char label
      (let* ((sorted-by-dist (sort (copy-sequence (flash-state-matches state))
                                   (lambda (a b)
                                     (< (marker-position (flash-match-pos a))
                                        (marker-position (flash-match-pos b))))))
             (farthest (car (last sorted-by-dist))))
        (should (= 2 (length (flash-match-label farthest))))))))

(ert-deftest flash-label-no-prefix-collision-test ()
  "Test that single-char labels don't share chars with multi-char prefixes.
With 4 chars (a,b,c,d) and 5 matches, P=1.
Singles: a,b,c (first 3 chars).  Prefix: d (last char).
No single-char label should equal any multi-char prefix."
  (with-temp-buffer
    (insert (make-string 100 ?x))
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let* ((flash-labels "abcd")
           (flash-label-uppercase nil)
           (state (flash-state-create (list (selected-window)))))
      (setf (flash-state-start-point state) 1)
      (setf (flash-state-matches state)
            (cl-loop for i from 1 to 5
                     collect (make-flash-match
                              :pos (copy-marker (* i 10))
                              :end-pos (copy-marker (1+ (* i 10)))
                              :label nil
                              :window (selected-window)
                              :fold nil)))
      (flash-label-matches state)
      (let* ((labels (mapcar #'flash-match-label (flash-state-matches state)))
             (single-labels (cl-remove-if-not (lambda (l) (= (length l) 1)) labels))
             (multi-labels (cl-remove-if-not (lambda (l) (> (length l) 1)) labels))
             (prefixes (delete-dups (mapcar (lambda (l) (substring l 0 1)) multi-labels))))
        ;; No single-char label should be a multi-char prefix
        (dolist (s single-labels)
          (should-not (member s prefixes)))))))

(ert-deftest flash-label-prefix-matching-test ()
  "Test prefix matching for multi-char labels."
  (with-temp-buffer
    (insert "test")
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window)))))
      (setf (flash-state-matches state)
            (list (make-flash-match
                   :pos (copy-marker 1) :end-pos (copy-marker 2)
                   :label "aa" :window (selected-window) :fold nil)
                  (make-flash-match
                   :pos (copy-marker 2) :end-pos (copy-marker 3)
                   :label "ab" :window (selected-window) :fold nil)
                  (make-flash-match
                   :pos (copy-marker 3) :end-pos (copy-marker 4)
                   :label "ba" :window (selected-window) :fold nil)))
      ;; All matches with "a" prefix
      (let ((matches (flash-matches-with-label-prefix state "a")))
        (should (= 2 (length matches)))  ; aa and ab
        (should (cl-every (lambda (m)
                            (string-prefix-p "a" (flash-match-label m)))
                          matches)))
      ;; All matches with "b" prefix
      (let ((matches (flash-matches-with-label-prefix state "b")))
        (should (= 1 (length matches)))  ; ba only
        (should (equal "ba" (flash-match-label (car matches))))))))

(provide 'flash-label-test)
;;; flash-label-test.el ends here
