;;; emacs-flash-jump-test.el --- Tests for emacs-flash-jump -*- lexical-binding: t -*-

;;; Commentary:
;; ERT tests for emacs-flash-jump module.

;;; Code:

(require 'ert)
(require 'emacs-flash-state)
(require 'emacs-flash-jump)

(ert-deftest emacs-flash-jump-to-match-test ()
  "Test jumping to a match."
  (with-temp-buffer
    (insert "foo bar baz")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((match (make-emacs-flash-match
                  :pos (copy-marker 5)
                  :end-pos (copy-marker 8)
                  :label "a"
                  :window (selected-window)
                  :fold nil)))
      (should (emacs-flash-jump-to-match match))
      (should (= 5 (point))))))

(ert-deftest emacs-flash-jump-to-label-test ()
  "Test jumping by label."
  (with-temp-buffer
    (insert "foo bar baz")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window)))))
      (setf (emacs-flash-state-matches state)
            (list (make-emacs-flash-match
                   :pos (copy-marker 1)
                   :end-pos (copy-marker 4)
                   :label "x"
                   :window (selected-window)
                   :fold nil)
                  (make-emacs-flash-match
                   :pos (copy-marker 9)
                   :end-pos (copy-marker 12)
                   :label "y"
                   :window (selected-window)
                   :fold nil)))
      ;; Jump to label 'y'
      (should (emacs-flash-jump-to-label state "y"))
      (should (= 9 (point)))
      ;; Jump to label 'x'
      (should (emacs-flash-jump-to-label state "x"))
      (should (= 1 (point))))))

(ert-deftest emacs-flash-jump-to-label-not-found-test ()
  "Test jump returns nil for non-existent label."
  (with-temp-buffer
    (insert "foo bar")
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window)))))
      (setf (emacs-flash-state-matches state)
            (list (make-emacs-flash-match
                   :pos (copy-marker 1)
                   :end-pos (copy-marker 4)
                   :label "a"
                   :window (selected-window)
                   :fold nil)))
      (should-not (emacs-flash-jump-to-label state "z")))))

(ert-deftest emacs-flash-jump-to-first-test ()
  "Test jumping to first match."
  (with-temp-buffer
    (insert "foo bar baz")
    (goto-char (point-max))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window)))))
      (setf (emacs-flash-state-matches state)
            (list (make-emacs-flash-match
                   :pos (copy-marker 5)
                   :end-pos (copy-marker 8)
                   :label "a"
                   :window (selected-window)
                   :fold nil)
                  (make-emacs-flash-match
                   :pos (copy-marker 9)
                   :end-pos (copy-marker 12)
                   :label "b"
                   :window (selected-window)
                   :fold nil)))
      (should (emacs-flash-jump-to-first state))
      (should (= 5 (point))))))

(ert-deftest emacs-flash-jump-to-first-no-matches-test ()
  "Test jump to first returns nil when no matches."
  (with-temp-buffer
    (insert "foo bar")
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window)))))
      (setf (emacs-flash-state-matches state) nil)
      (should-not (emacs-flash-jump-to-first state)))))

(ert-deftest emacs-flash-return-to-start-test ()
  "Test returning to start position."
  (with-temp-buffer
    (insert "foo bar baz")
    (goto-char 5)
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window)))))
      (setf (emacs-flash-state-start-point state) 5)
      (setf (emacs-flash-state-start-window state) (selected-window))
      ;; Move somewhere else
      (goto-char (point-max))
      ;; Return to start
      (emacs-flash-return-to-start state)
      (should (= 5 (point))))))

(ert-deftest emacs-flash-jump-to-nil-match-test ()
  "Test jump to nil match returns nil."
  (should-not (emacs-flash-jump-to-match nil)))

(ert-deftest emacs-flash-jump-find-match-by-label-test ()
  "Test finding match by label in jump module."
  (with-temp-buffer
    (insert "test")
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window))))
          (match1 (make-emacs-flash-match
                   :pos (copy-marker 1)
                   :end-pos (copy-marker 2)
                   :label "a"
                   :window (selected-window)
                   :fold nil))
          (match2 (make-emacs-flash-match
                   :pos (copy-marker 3)
                   :end-pos (copy-marker 4)
                   :label "b"
                   :window (selected-window)
                   :fold nil)))
      (setf (emacs-flash-state-matches state) (list match1 match2))
      (should (eq match1 (emacs-flash-find-match-by-label state "a")))
      (should (eq match2 (emacs-flash-find-match-by-label state "b")))
      (should-not (emacs-flash-find-match-by-label state "c")))))

;;; Jump Position Tests

(ert-deftest emacs-flash-jump-position-start-test ()
  "Test jump to start of match (default)."
  (with-temp-buffer
    (insert "foo bar baz")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((emacs-flash-jump-position 'start)
          (match (make-emacs-flash-match
                  :pos (copy-marker 5)
                  :end-pos (copy-marker 8)
                  :label "a"
                  :window (selected-window)
                  :fold nil)))
      (emacs-flash-jump-to-match match)
      (should (= 5 (point))))))

(ert-deftest emacs-flash-jump-position-end-test ()
  "Test jump to end of match."
  (with-temp-buffer
    (insert "foo bar baz")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((emacs-flash-jump-position 'end)
          (match (make-emacs-flash-match
                  :pos (copy-marker 5)
                  :end-pos (copy-marker 8)
                  :label "a"
                  :window (selected-window)
                  :fold nil)))
      (emacs-flash-jump-to-match match)
      (should (= 8 (point))))))

(ert-deftest emacs-flash-jump-position-defcustom-test ()
  "Test that jump-position defcustom exists."
  (should (boundp 'emacs-flash-jump-position)))

(provide 'emacs-flash-jump-test)
;;; emacs-flash-jump-test.el ends here
