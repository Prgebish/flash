;;; flash-jump-test.el --- Tests for flash-jump -*- lexical-binding: t -*-

;;; Commentary:
;; ERT tests for flash-jump module.

;;; Code:

(require 'ert)
(require 'flash-state)
(require 'flash-jump)

(ert-deftest flash-jump-to-match-test ()
  "Test jumping to a match."
  (with-temp-buffer
    (insert "foo bar baz")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((match (make-flash-match
                  :pos (copy-marker 5)
                  :end-pos (copy-marker 8)
                  :label "a"
                  :window (selected-window)
                  :fold nil)))
      (should (flash-jump-to-match match))
      (should (= 5 (point))))))

(ert-deftest flash-jump-to-label-test ()
  "Test jumping by label."
  (with-temp-buffer
    (insert "foo bar baz")
    (goto-char (point-min))
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
                   :pos (copy-marker 9)
                   :end-pos (copy-marker 12)
                   :label "y"
                   :window (selected-window)
                   :fold nil)))
      ;; Jump to label 'y'
      (should (flash-jump-to-label state "y"))
      (should (= 9 (point)))
      ;; Jump to label 'x'
      (should (flash-jump-to-label state "x"))
      (should (= 1 (point))))))

(ert-deftest flash-jump-to-label-not-found-test ()
  "Test jump returns nil for non-existent label."
  (with-temp-buffer
    (insert "foo bar")
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window)))))
      (setf (flash-state-matches state)
            (list (make-flash-match
                   :pos (copy-marker 1)
                   :end-pos (copy-marker 4)
                   :label "a"
                   :window (selected-window)
                   :fold nil)))
      (should-not (flash-jump-to-label state "z")))))

(ert-deftest flash-jump-to-first-test ()
  "Test jumping to first match."
  (with-temp-buffer
    (insert "foo bar baz")
    (goto-char (point-max))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window)))))
      (setf (flash-state-matches state)
            (list (make-flash-match
                   :pos (copy-marker 5)
                   :end-pos (copy-marker 8)
                   :label "a"
                   :window (selected-window)
                   :fold nil)
                  (make-flash-match
                   :pos (copy-marker 9)
                   :end-pos (copy-marker 12)
                   :label "b"
                   :window (selected-window)
                   :fold nil)))
      (should (flash-jump-to-first state))
      (should (= 5 (point))))))

(ert-deftest flash-jump-to-first-no-matches-test ()
  "Test jump to first returns nil when no matches."
  (with-temp-buffer
    (insert "foo bar")
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window)))))
      (setf (flash-state-matches state) nil)
      (should-not (flash-jump-to-first state)))))

(ert-deftest flash-return-to-start-test ()
  "Test returning to start position."
  (with-temp-buffer
    (insert "foo bar baz")
    (goto-char 5)
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window)))))
      (setf (flash-state-start-point state) 5)
      (setf (flash-state-start-window state) (selected-window))
      ;; Move somewhere else
      (goto-char (point-max))
      ;; Return to start
      (flash-return-to-start state)
      (should (= 5 (point))))))

(ert-deftest flash-jump-to-nil-match-test ()
  "Test jump to nil match returns nil."
  (should-not (flash-jump-to-match nil)))

(ert-deftest flash-jump-find-match-by-label-test ()
  "Test finding match by label in jump module."
  (with-temp-buffer
    (insert "test")
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window))))
          (match1 (make-flash-match
                   :pos (copy-marker 1)
                   :end-pos (copy-marker 2)
                   :label "a"
                   :window (selected-window)
                   :fold nil))
          (match2 (make-flash-match
                   :pos (copy-marker 3)
                   :end-pos (copy-marker 4)
                   :label "b"
                   :window (selected-window)
                   :fold nil)))
      (setf (flash-state-matches state) (list match1 match2))
      (should (eq match1 (flash-find-match-by-label state "a")))
      (should (eq match2 (flash-find-match-by-label state "b")))
      (should-not (flash-find-match-by-label state "c")))))

;;; Jump Position Tests

(ert-deftest flash-jump-position-start-test ()
  "Test jump to start of match (default)."
  (with-temp-buffer
    (insert "foo bar baz")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((flash-jump-position 'start)
          (match (make-flash-match
                  :pos (copy-marker 5)
                  :end-pos (copy-marker 8)
                  :label "a"
                  :window (selected-window)
                  :fold nil)))
      (flash-jump-to-match match)
      (should (= 5 (point))))))

(ert-deftest flash-jump-position-end-test ()
  "Test jump to end of match."
  (with-temp-buffer
    (insert "foo bar baz")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((flash-jump-position 'end)
          (match (make-flash-match
                  :pos (copy-marker 5)
                  :end-pos (copy-marker 8)
                  :label "a"
                  :window (selected-window)
                  :fold nil)))
      (flash-jump-to-match match)
      (should (= 8 (point))))))

(ert-deftest flash-jump-position-defcustom-test ()
  "Test that jump-position defcustom exists."
  (should (boundp 'flash-jump-position)))

;;; Mark / Region Preservation Tests

(ert-deftest flash-jump-preserves-mark-when-region-active-test ()
  "Test that jump does not overwrite mark when region is active.
When `transient-mark-mode' and `mark-active' are both true,
`push-mark' should be skipped to preserve the selection anchor."
  (with-temp-buffer
    (insert "foo bar baz")
    (set-window-buffer (selected-window) (current-buffer))
    (let ((flash-jumplist t)
          (transient-mark-mode t))
      ;; Set mark at position 1, move point to 5 to create a region
      (goto-char 1)
      (push-mark 1 t t)            ; activate mark
      (goto-char 5)
      (should mark-active)
      (let ((mark-before (mark)))
        (flash-jump-to-match
         (make-flash-match
          :pos (copy-marker 9)
          :end-pos (copy-marker 12)
          :label "a"
          :window (selected-window)
          :fold nil))
        ;; Mark should stay at original position, not be overwritten to 5
        (should (= mark-before (mark)))))))

(ert-deftest flash-jump-pushes-mark-when-no-region-test ()
  "Test that jump pushes mark when no region is active."
  (with-temp-buffer
    (insert "foo bar baz")
    (set-window-buffer (selected-window) (current-buffer))
    (let ((flash-jumplist t))
      (goto-char 5)
      (deactivate-mark)
      (should-not mark-active)
      (flash-jump-to-match
       (make-flash-match
        :pos (copy-marker 9)
        :end-pos (copy-marker 12)
        :label "a"
        :window (selected-window)
        :fold nil))
      ;; Mark should have been pushed at pre-jump position (5)
      (should (= 5 (mark))))))

(provide 'flash-jump-test)
;;; flash-jump-test.el ends here
