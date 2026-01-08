;;; emacs-flash-line-test.el --- Tests for emacs-flash-line -*- lexical-binding: t -*-

;;; Commentary:
;; ERT tests for emacs-flash-line module.

;;; Code:

(require 'ert)
(require 'emacs-flash-state)
(require 'emacs-flash-line)

(ert-deftest emacs-flash-line-collect-lines-test ()
  "Test that line mode collects all visible lines."
  (with-temp-buffer
    (insert "line 1\nline 2\nline 3\nline 4\nline 5")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window)))))
      (emacs-flash-line--collect-lines state)
      ;; Should have 5 matches (one per line)
      (should (= 5 (length (emacs-flash-state-matches state))))
      ;; First match should be at position 1
      (should (= 1 (marker-position
                    (emacs-flash-match-pos
                     (car (emacs-flash-state-matches state)))))))))

(ert-deftest emacs-flash-line-matches-at-line-start-test ()
  "Test that matches are at line beginnings."
  (with-temp-buffer
    (insert "  indented\nnormal\n  also indented")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window)))))
      (emacs-flash-line--collect-lines state)
      ;; All matches should be at column 0
      (dolist (match (emacs-flash-state-matches state))
        (let ((pos (marker-position (emacs-flash-match-pos match))))
          (save-excursion
            (goto-char pos)
            (should (= 0 (current-column)))))))))

(ert-deftest emacs-flash-line-empty-buffer-test ()
  "Test line mode with empty buffer."
  (with-temp-buffer
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window)))))
      (emacs-flash-line--collect-lines state)
      ;; Empty buffer has one "line" at point-min
      (should (<= (length (emacs-flash-state-matches state)) 1)))))

(ert-deftest emacs-flash-line-single-line-test ()
  "Test line mode with single line buffer."
  (with-temp-buffer
    (insert "single line no newline")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window)))))
      (emacs-flash-line--collect-lines state)
      ;; Should have exactly 1 match
      (should (= 1 (length (emacs-flash-state-matches state)))))))

(ert-deftest emacs-flash-line-command-exists-test ()
  "Test that line jump command exists."
  (should (fboundp 'emacs-flash-line-jump)))

(ert-deftest emacs-flash-line-labels-assigned-test ()
  "Test that labels are assigned to line matches."
  (with-temp-buffer
    (insert "line 1\nline 2\nline 3")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window)))))
      (emacs-flash-line--collect-lines state)
      (emacs-flash-label-matches state)
      ;; All matches should have labels
      (dolist (match (emacs-flash-state-matches state))
        (should (emacs-flash-match-label match))))))

(ert-deftest emacs-flash-line-valid-prefix-test ()
  "Test prefix validation for multi-char labels."
  (with-temp-buffer
    (insert "test")
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (emacs-flash-state-create (list (selected-window)))))
      ;; Set up multi-char labels manually
      (setf (emacs-flash-state-matches state)
            (list (make-emacs-flash-match
                   :pos (copy-marker 1) :end-pos (copy-marker 2)
                   :label "aa" :window (selected-window) :fold nil)
                  (make-emacs-flash-match
                   :pos (copy-marker 2) :end-pos (copy-marker 3)
                   :label "ab" :window (selected-window) :fold nil)))
      ;; "a" is a valid prefix
      (should (emacs-flash-line--valid-prefix-p state "a"))
      ;; "b" is not a valid prefix
      (should-not (emacs-flash-line--valid-prefix-p state "b")))))

(provide 'emacs-flash-line-test)
;;; emacs-flash-line-test.el ends here
