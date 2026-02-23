;;; flash-label-test.el --- Tests for flash-label -*- lexical-binding: t -*-

;;; Commentary:
;; ERT tests for flash-label module.

;;; Code:

(require 'ert)
(require 'flash-state)
(require 'flash-search)
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
      ;; Populate matches so continuation-chars can read char-after
      (flash-search state)
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

(ert-deftest flash-label-max-matches-limit-test ()
  "Test that MAX-MATCHES limits labels to nearest candidates."
  (with-temp-buffer
    (insert (make-string 40 ?x))
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let* ((flash-labels "abcd")
           (flash-label-uppercase nil)
           (flash-multi-char-labels nil)
           (state (flash-state-create (list (selected-window)))))
      (setf (flash-state-pattern state) "")
      (setf (flash-state-start-point state) 10)
      ;; Matches at positions with unique distances from point 10:
      ;; 8 (2), 13 (3), 4 (6), 19 (9), 1 (9)
      (setf (flash-state-matches state)
            (mapcar (lambda (pos)
                      (make-flash-match
                       :pos (copy-marker pos)
                       :end-pos (copy-marker (1+ pos))
                       :label nil
                       :window (selected-window)
                       :fold nil))
                    '(1 4 8 13 19)))
      (flash-label-matches state 3)
      (let ((labeled-positions
             (sort (mapcar #'flash-match-pos-value
                           (cl-remove-if-not #'flash-match-label
                                             (flash-state-matches state)))
                   #'<)))
        (should (equal '(4 8 13) labeled-positions))
        (should (= 3 (hash-table-count (flash-state-label-index state))))))))

(ert-deftest flash-label-more-matches-than-labels-test ()
  "Test multi-char labels when matches exceed single-char capacity.
With 2 label chars (a,b), we can generate 4 two-char labels (aa,ab,ba,bb)."
  (with-temp-buffer
    (insert (make-string 100 ?x))  ; 100 x's
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let* ((flash-labels "ab")  ; 2 chars = 4 two-char labels
           (flash-label-uppercase nil)  ; no uppercase for this test
           (flash-multi-char-labels t)  ; enabled
           (state (flash-state-create (list (selected-window)))))
      ;; Create 5 matches
      (setf (flash-state-matches state)
            (cl-loop for i from 1 to 5
                     collect (make-flash-match
                              :pos (copy-marker (* i 10))
                              :end-pos (copy-marker (1+ (* i 10)))
                              :label nil
                              :window (selected-window)
                              :fold nil)))
      (flash-label-matches state)
      ;; 4 matches should have two-char labels (aa, ab, ba, bb)
      (let ((labeled (cl-count-if #'flash-match-label
                                  (flash-state-matches state))))
        (should (= 4 labeled)))
      ;; Labels should be multi-char
      (let ((first-label (flash-match-label
                          (car (flash-state-matches state)))))
        (should (= 2 (length first-label)))))))

(ert-deftest flash-label-single-char-only-test ()
  "Test that multi-char labels are disabled when flash-multi-char-labels is nil.
With 2 label chars (a,b) and 5 matches, only 2 should get labels."
  (with-temp-buffer
    (insert (make-string 100 ?x))  ; 100 x's
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let* ((flash-labels "ab")  ; 2 chars
           (flash-label-uppercase nil)  ; no uppercase for this test
           (flash-multi-char-labels nil)  ; disabled
           (state (flash-state-create (list (selected-window)))))
      ;; Create 5 matches
      (setf (flash-state-matches state)
            (cl-loop for i from 1 to 5
                     collect (make-flash-match
                              :pos (copy-marker (* i 10))
                              :end-pos (copy-marker (1+ (* i 10)))
                              :label nil
                              :window (selected-window)
                              :fold nil)))
      (flash-label-matches state)
      ;; Only 2 matches should have labels (single-char only)
      (let ((labeled (cl-count-if #'flash-match-label
                                  (flash-state-matches state))))
        (should (= 2 labeled)))
      ;; Labels should be single-char
      (let ((first-label (flash-match-label
                          (car (flash-state-matches state)))))
        (should (= 1 (length first-label)))))))

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

(ert-deftest flash-continuation-chars-from-matches-test ()
  "Test that continuation chars are collected from match end positions."
  (with-temp-buffer
    (insert "ab abc abd")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((flash-case-fold nil)
          (state (flash-state-create (list (selected-window)))))
      (setf (flash-state-pattern state) "ab")
      (flash-search state)
      (let ((chars (flash--continuation-chars state)))
        ;; Space after "ab", 'c' after "ab" in "abc", 'd' after "ab" in "abd"
        (should (gethash ?\s chars))
        (should (gethash ?c chars))
        (should (gethash ?d chars))
        ;; 'a' should not be present
        (should-not (gethash ?a chars))))))

(ert-deftest flash-continuation-chars-case-fold-test ()
  "Test that continuation chars respect case folding."
  (with-temp-buffer
    (insert "ab ABC")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((flash-case-fold t)
          (state (flash-state-create (list (selected-window)))))
      (setf (flash-state-pattern state) "ab")
      (flash-search state)
      (let ((chars (flash--continuation-chars state)))
        ;; With case-fold, 'C' from "ABC" should be stored as 'c'
        (should (gethash ?c chars))
        ;; Space after first "ab"
        (should (gethash ?\s chars))))))

(ert-deftest flash-continuation-chars-end-of-buffer-test ()
  "Test that matches at end of buffer don't cause errors."
  (with-temp-buffer
    (insert "ab")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((flash-case-fold nil)
          (state (flash-state-create (list (selected-window)))))
      (setf (flash-state-pattern state) "ab")
      (flash-search state)
      ;; Should not error; match at EOB has no continuation char
      (let ((chars (flash--continuation-chars state)))
        (should (hash-table-p chars))
        (should (= 0 (hash-table-count chars)))))))

(ert-deftest flash-label-fold-dedup-test ()
  "Test that only one label is assigned per fold.
Matches sharing the same fold value should be deduplicated,
keeping only the closest match to the cursor."
  (with-temp-buffer
    (insert (make-string 40 ?x))
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let* ((flash-labels "abcdef")
           (flash-label-uppercase nil)
           (flash-multi-char-labels nil)
           (state (flash-state-create (list (selected-window)))))
      (setf (flash-state-pattern state) "")
      (setf (flash-state-start-point state) 1)
      ;; Three matches in the same fold (fold = 5), one without fold
      (setf (flash-state-matches state)
            (list (make-flash-match
                   :pos (copy-marker 5) :end-pos (copy-marker 6)
                   :label nil :window (selected-window) :fold 5)
                  (make-flash-match
                   :pos (copy-marker 10) :end-pos (copy-marker 11)
                   :label nil :window (selected-window) :fold 5)
                  (make-flash-match
                   :pos (copy-marker 20) :end-pos (copy-marker 21)
                   :label nil :window (selected-window) :fold 5)
                  (make-flash-match
                   :pos (copy-marker 30) :end-pos (copy-marker 31)
                   :label nil :window (selected-window) :fold nil)))
      (flash-label-matches state)
      ;; Only 2 matches should have labels: closest from fold + non-fold match
      (let ((labeled (cl-remove-if-not #'flash-match-label
                                       (flash-state-matches state))))
        (should (= 2 (length labeled))))
      ;; The closest fold match (pos 5, dist 4) should have the label
      (should (flash-match-label
               (cl-find 5 (flash-state-matches state)
                        :key (lambda (m) (marker-position (flash-match-pos m))))))
      ;; The further fold matches should NOT have labels
      (should-not (flash-match-label
                   (cl-find 10 (flash-state-matches state)
                            :key (lambda (m) (marker-position (flash-match-pos m))))))
      (should-not (flash-match-label
                   (cl-find 20 (flash-state-matches state)
                            :key (lambda (m) (marker-position (flash-match-pos m)))))))))

(provide 'flash-label-test)
;;; flash-label-test.el ends here
