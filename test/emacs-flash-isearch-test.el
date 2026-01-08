;;; emacs-flash-isearch-test.el --- Tests for emacs-flash-isearch -*- lexical-binding: t -*-

;;; Commentary:
;; ERT tests for emacs-flash-isearch module.

;;; Code:

(require 'ert)
(require 'emacs-flash)
(require 'emacs-flash-isearch)

;;; State Management Tests

(ert-deftest emacs-flash-isearch-start-test ()
  "Test that start creates state correctly."
  (with-temp-buffer
    (insert "hello world")
    (set-window-buffer (selected-window) (current-buffer))
    (let ((emacs-flash-isearch-enabled t))
      (emacs-flash-isearch--start)
      (should emacs-flash-isearch--active)
      (should emacs-flash-isearch--state)
      (should (eq emacs-flash-isearch--original-buffer (current-buffer)))
      ;; Cleanup
      (emacs-flash-isearch--stop))))

(ert-deftest emacs-flash-isearch-stop-test ()
  "Test that stop cleans up properly."
  (with-temp-buffer
    (insert "hello world")
    (set-window-buffer (selected-window) (current-buffer))
    (let ((emacs-flash-isearch-enabled t))
      (emacs-flash-isearch--start)
      (emacs-flash-isearch--stop)
      (should-not emacs-flash-isearch--active)
      (should-not emacs-flash-isearch--state)
      (should-not emacs-flash-isearch--original-buffer))))

(ert-deftest emacs-flash-isearch-disabled-test ()
  "Test that nothing happens when disabled."
  (with-temp-buffer
    (insert "hello world")
    (set-window-buffer (selected-window) (current-buffer))
    (let ((emacs-flash-isearch-enabled nil))
      (emacs-flash-isearch--start)
      (should-not emacs-flash-isearch--active)
      (should-not emacs-flash-isearch--state))))

;;; Update Tests

(ert-deftest emacs-flash-isearch-update-test ()
  "Test that update finds matches and creates labels."
  (with-temp-buffer
    (insert "foo bar foo baz foo")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((emacs-flash-isearch-enabled t))
      (emacs-flash-isearch--start)
      (emacs-flash-isearch--update "foo")
      ;; Should have 3 matches
      (should (= 3 (length (emacs-flash-state-matches
                            emacs-flash-isearch--state))))
      ;; First match should have label
      (should (emacs-flash-match-label
               (car (emacs-flash-state-matches
                     emacs-flash-isearch--state))))
      ;; Cleanup
      (emacs-flash-isearch--stop))))

(ert-deftest emacs-flash-isearch-update-empty-pattern-test ()
  "Test that empty pattern doesn't cause errors."
  (with-temp-buffer
    (insert "hello world")
    (set-window-buffer (selected-window) (current-buffer))
    (let ((emacs-flash-isearch-enabled t))
      (emacs-flash-isearch--start)
      ;; Should not error on empty pattern
      (emacs-flash-isearch--update "")
      (emacs-flash-isearch--stop))))

;;; Toggle Tests

(ert-deftest emacs-flash-isearch-toggle-test ()
  "Test toggle functionality."
  (with-temp-buffer
    (insert "hello world")
    (set-window-buffer (selected-window) (current-buffer))
    (let ((emacs-flash-isearch-enabled t))
      (emacs-flash-isearch--start)
      (should emacs-flash-isearch--active)
      ;; Toggle off
      (emacs-flash-isearch--toggle)
      (should-not emacs-flash-isearch--active)
      ;; Toggle on
      (emacs-flash-isearch--toggle)
      (should emacs-flash-isearch--active)
      ;; Cleanup
      (emacs-flash-isearch--stop))))

;;; Jump Tests

(ert-deftest emacs-flash-isearch-try-jump-test ()
  "Test jumping to a label."
  (with-temp-buffer
    (insert "aaa bbb aaa ccc aaa")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((emacs-flash-isearch-enabled t))
      (emacs-flash-isearch--start)
      (emacs-flash-isearch--update "aaa")
      ;; Get label of second match
      (let* ((matches (emacs-flash-state-matches emacs-flash-isearch--state))
             (second-match (nth 1 matches))
             (label (emacs-flash-match-label second-match))
             (expected-pos (marker-position (emacs-flash-match-pos second-match))))
        (when label
          ;; Try jump (sets pending match)
          (emacs-flash-isearch--try-jump label)
          ;; Simulate search exit - do pending jump
          (emacs-flash-isearch--do-pending-jump)
          ;; Should be at second match position
          (should (= expected-pos (point))))))))

(ert-deftest emacs-flash-isearch-try-jump-invalid-label-test ()
  "Test that invalid label doesn't jump."
  (with-temp-buffer
    (insert "foo bar foo")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((emacs-flash-isearch-enabled t)
          (start-pos (point)))
      (emacs-flash-isearch--start)
      (emacs-flash-isearch--update "foo")
      ;; Try to jump with invalid label
      (should-not (emacs-flash-isearch--try-jump ?z))
      ;; Position shouldn't change
      (should (= start-pos (point)))
      ;; Cleanup
      (emacs-flash-isearch--stop))))

;;; Defcustom Tests

(ert-deftest emacs-flash-isearch-defcustom-enabled-test ()
  "Test that enabled defcustom exists."
  (should (boundp 'emacs-flash-isearch-enabled)))

(ert-deftest emacs-flash-isearch-defcustom-toggle-key-test ()
  "Test that toggle-key defcustom exists."
  (should (boundp 'emacs-flash-isearch-toggle-key)))

;;; Mode Tests

(ert-deftest emacs-flash-isearch-mode-exists-test ()
  "Test that the minor mode exists."
  (should (fboundp 'emacs-flash-isearch-mode)))

(provide 'emacs-flash-isearch-test)
;;; emacs-flash-isearch-test.el ends here
