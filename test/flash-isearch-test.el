;;; flash-isearch-test.el --- Tests for flash-isearch -*- lexical-binding: t -*-

;;; Commentary:
;; ERT tests for flash-isearch module.

;;; Code:

(require 'ert)
(require 'flash)
(require 'flash-isearch)

;;; State Management Tests

(ert-deftest flash-isearch-start-test ()
  "Test that start creates state correctly."
  (with-temp-buffer
    (insert "hello world")
    (set-window-buffer (selected-window) (current-buffer))
    (let ((flash-isearch-enabled t))
      (flash-isearch--start)
      (should flash-isearch--active)
      (should flash-isearch--state)
      (should (eq flash-isearch--original-buffer (current-buffer)))
      ;; Cleanup
      (flash-isearch--stop))))

(ert-deftest flash-isearch-stop-test ()
  "Test that stop cleans up properly."
  (with-temp-buffer
    (insert "hello world")
    (set-window-buffer (selected-window) (current-buffer))
    (let ((flash-isearch-enabled t))
      (flash-isearch--start)
      (flash-isearch--stop)
      (should-not flash-isearch--active)
      (should-not flash-isearch--state)
      (should-not flash-isearch--original-buffer))))

(ert-deftest flash-isearch-disabled-test ()
  "Test that session is created but labels inactive when disabled."
  (with-temp-buffer
    (insert "hello world")
    (set-window-buffer (selected-window) (current-buffer))
    (let ((flash-isearch-enabled nil))
      (flash-isearch--start)
      (should-not flash-isearch--active)
      ;; State is created so toggle can activate labels later
      (should flash-isearch--state)
      (should flash-isearch--in-session)
      ;; Cleanup
      (flash-isearch--stop))))

;;; Update Tests

(ert-deftest flash-isearch-update-test ()
  "Test that update finds matches and creates labels."
  (with-temp-buffer
    (insert "foo bar foo baz foo")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((flash-isearch-enabled t))
      (flash-isearch--start)
      (flash-isearch--update "foo")
      ;; Should have 3 matches
      (should (= 3 (length (flash-state-matches
                            flash-isearch--state))))
      ;; First match should have label
      (should (flash-match-label
               (car (flash-state-matches
                     flash-isearch--state))))
      ;; Cleanup
      (flash-isearch--stop))))

(ert-deftest flash-isearch-update-empty-pattern-test ()
  "Test that empty pattern clears matches and overlays."
  (with-temp-buffer
    (insert "hello world")
    (set-window-buffer (selected-window) (current-buffer))
    (let ((flash-isearch-enabled t))
      (flash-isearch--start)
      (flash-isearch--update "hello")
      (should (flash-state-matches flash-isearch--state))
      (should (flash-state-overlays flash-isearch--state))
      (flash-isearch--update "")
      (should-not (flash-state-matches flash-isearch--state))
      (should-not (flash-state-overlays flash-isearch--state))
      (flash-isearch--stop))))

;;; Toggle Tests

(ert-deftest flash-isearch-toggle-test ()
  "Test toggle functionality."
  (with-temp-buffer
    (insert "hello world")
    (set-window-buffer (selected-window) (current-buffer))
    (let ((flash-isearch-enabled t))
      (flash-isearch--start)
      (should flash-isearch--active)
      ;; Toggle off
      (flash-isearch--toggle)
      (should-not flash-isearch--active)
      ;; Toggle on
      (flash-isearch--toggle)
      (should flash-isearch--active)
      ;; Cleanup
      (flash-isearch--stop))))

(ert-deftest flash-isearch-toggle-from-disabled-test ()
  "Test that toggle activates labels when isearch-enabled is nil."
  (with-temp-buffer
    (insert "foo bar foo baz foo")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((flash-isearch-enabled nil)
          (isearch-mode t)
          (isearch-string "foo"))
      (flash-isearch--start)
      (should-not flash-isearch--active)
      ;; Toggle on — should activate and show labels
      (flash-isearch--toggle)
      (should flash-isearch--active)
      (should (> (length (flash-state-matches flash-isearch--state)) 0))
      ;; Cleanup
      (flash-isearch--stop))))

;;; Jump Tests

(ert-deftest flash-isearch-try-jump-test ()
  "Test jumping to a label."
  (with-temp-buffer
    (insert "aaa bbb aaa ccc aaa")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((flash-isearch-enabled t))
      (flash-isearch--start)
      (flash-isearch--update "aaa")
      ;; Get label of second match
      (let* ((matches (flash-state-matches flash-isearch--state))
             (second-match (nth 1 matches))
             (label (flash-match-label second-match))
             (expected-pos (marker-position (flash-match-pos second-match))))
        (when label
          ;; Try jump (sets pending match) - pass char, not string
          (flash-isearch--try-jump (string-to-char label))
          ;; Simulate search exit - do pending jump
          (flash-isearch--do-pending-jump)
          ;; Should be at second match position
          (should (= expected-pos (point))))))))

(ert-deftest flash-isearch-try-jump-invalid-label-test ()
  "Test that invalid label doesn't jump."
  (with-temp-buffer
    (insert "foo bar foo")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((flash-isearch-enabled t)
          (start-pos (point)))
      (flash-isearch--start)
      (flash-isearch--update "foo")
      ;; Try to jump with invalid label
      (should-not (flash-isearch--try-jump ?z))
      ;; Position shouldn't change
      (should (= start-pos (point)))
      ;; Cleanup
      (flash-isearch--stop))))

;;; Defcustom Tests

(ert-deftest flash-isearch-defcustom-enabled-test ()
  "Test that enabled defcustom exists."
  (should (boundp 'flash-isearch-enabled)))

(ert-deftest flash-isearch-defcustom-toggle-key-test ()
  "Test that toggle-key defcustom exists."
  (should (boundp 'flash-isearch-toggle-key)))

(ert-deftest flash-isearch-defcustom-trigger-test ()
  "Test that trigger defcustom exists and defaults to nil (smart skip)."
  (should (boundp 'flash-isearch-trigger))
  (should (null flash-isearch-trigger)))

(ert-deftest flash-isearch-trigger-char-validation-test ()
  "Test trigger conversion only accepts one-character strings."
  (let ((flash-isearch-trigger ""))
    (should-not (flash-isearch--trigger-char)))
  (let ((flash-isearch-trigger "ab"))
    (should-not (flash-isearch--trigger-char)))
  (let ((flash-isearch-trigger ";"))
    (should (= ?\; (flash-isearch--trigger-char)))))

;;; Mode Tests

(ert-deftest flash-isearch-mode-exists-test ()
  "Test that the minor mode exists."
  (should (fboundp 'flash-isearch-mode)))

(provide 'flash-isearch-test)
;;; flash-isearch-test.el ends here
