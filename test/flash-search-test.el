;;; flash-search-test.el --- Tests for flash-search -*- lexical-binding: t -*-

;;; Commentary:
;; ERT tests for flash-search module.

;;; Code:

(require 'ert)
(require 'flash-state)
(require 'flash-search)

(ert-deftest flash-search-empty-pattern-test ()
  "Test search with empty pattern returns no matches."
  (with-temp-buffer
    (insert "foo bar baz")
    (let ((state (flash-state-create (list (selected-window)))))
      (setf (flash-state-pattern state) "")
      (flash-search state)
      (should (null (flash-state-matches state))))))

(ert-deftest flash-search-basic-test ()
  "Test basic search finds matches."
  (with-temp-buffer
    (insert "foo bar foo baz foo")
    (goto-char (point-min))
    ;; Need to display buffer in window for window-start/end to work
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window)))))
      (setf (flash-state-pattern state) "foo")
      (flash-search state)
      (should (= 3 (length (flash-state-matches state))))
      ;; Check first match
      (let ((first-match (car (flash-state-matches state))))
        (should (= 1 (flash-match-pos-value first-match)))
        (should (= 4 (flash-match-end-pos-value first-match)))
        (should (eq (current-buffer) (flash-match-buffer first-match)))
        (should (eq (selected-window) (flash-match-window first-match)))))))

(ert-deftest flash-search-case-insensitive-test ()
  "Test search is case-insensitive."
  (with-temp-buffer
    (insert "Foo FOO foo")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window)))))
      (setf (flash-state-pattern state) "foo")
      (flash-search state)
      (should (= 3 (length (flash-state-matches state)))))))

(ert-deftest flash-search-no-match-test ()
  "Test search with no matches."
  (with-temp-buffer
    (insert "foo bar baz")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window)))))
      (setf (flash-state-pattern state) "xyz")
      (flash-search state)
      (should (null (flash-state-matches state))))))

(ert-deftest flash-search-position-storage-test ()
  "Test that matches use lightweight numeric positions."
  (with-temp-buffer
    (insert "test")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window)))))
      (setf (flash-state-pattern state) "test")
      (flash-search state)
      (let ((match (car (flash-state-matches state))))
        (should (integerp (flash-match-pos match)))
        (should (integerp (flash-match-end-pos match)))
        (should (eq (current-buffer) (flash-match-buffer-live match)))))))

(ert-deftest flash-search-skips-dead-window-test ()
  "Test search skips windows that were deleted during a flash session."
  (let ((buf (generate-new-buffer "*flash-search-dead-window*"))
        win)
    (unwind-protect
        (progn
          (setq win (split-window-right))
          (with-current-buffer buf
            (insert "foo bar"))
          (set-window-buffer win buf)
          (delete-window win)
          (let ((state (flash-state-create (list win))))
            (setf (flash-state-pattern state) "foo")
            (flash-search state)
            (should-not (flash-state-matches state))))
      (when (window-live-p win)
        (delete-window win))
      (kill-buffer buf))))

(ert-deftest flash-get-fold-at-visible-test ()
  "Test fold detection returns nil for visible text."
  (with-temp-buffer
    (insert "visible text")
    (should (null (flash--get-fold-at 5)))))

(ert-deftest flash-search-dedup-same-buffer-test ()
  "Test that duplicate positions from multiple windows are deduplicated.
When two windows show the same buffer, each position should appear only once."
  (let ((buf (generate-new-buffer "*flash-search-dedup*"))
        win2)
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "foo bar foo baz foo"))
          (set-window-buffer (selected-window) buf)
          (setq win2 (split-window-right))
          (set-window-buffer win2 buf)
          (let ((state (flash-state-create (list (selected-window) win2))))
            (setf (flash-state-pattern state) "foo")
            (flash-search state)
            ;; Should find 3 matches (not 6), deduplicating across windows
            (should (= 3 (length (flash-state-matches state))))))
      (when (window-live-p win2)
        (delete-window win2))
      (kill-buffer buf))))

(provide 'flash-search-test)
;;; flash-search-test.el ends here
