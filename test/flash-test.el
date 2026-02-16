;;; flash-test.el --- Tests for flash -*- lexical-binding: t -*-

;;; Commentary:
;; ERT tests for flash main module.

;;; Code:

(require 'ert)
(require 'flash)

(ert-deftest flash-format-prompt-empty-test ()
  "Test prompt formatting with empty pattern."
  (should (string= "Flash: " (flash--format-prompt "" 0))))

(ert-deftest flash-format-prompt-with-pattern-test ()
  "Test prompt formatting with pattern."
  (should (string= "Flash [foo] (3): " (flash--format-prompt "foo" 3))))

(ert-deftest flash-defcustom-labels-test ()
  "Test that labels defcustom exists and has default value."
  (should (boundp 'flash-labels))
  (should (stringp flash-labels))
  (should (> (length flash-labels) 0)))

(ert-deftest flash-defcustom-multi-window-test ()
  "Test that multi-window defcustom exists."
  (should (boundp 'flash-multi-window))
  (should (booleanp flash-multi-window)))

(ert-deftest flash-defcustom-autojump-test ()
  "Test that autojump defcustom exists."
  (should (boundp 'flash-autojump))
  (should (booleanp flash-autojump)))

(ert-deftest flash-defcustom-backdrop-test ()
  "Test that backdrop defcustom exists."
  (should (boundp 'flash-backdrop))
  (should (booleanp flash-backdrop)))

(ert-deftest flash-defcustom-case-fold-test ()
  "Test that case-fold defcustom exists."
  (should (boundp 'flash-case-fold))
  (should (booleanp flash-case-fold)))

(ert-deftest flash-jump-command-exists-test ()
  "Test that flash-jump command exists."
  (should (fboundp 'flash-jump))
  (should (commandp 'flash-jump)))

(ert-deftest flash-defcustom-rainbow-test ()
  "Test that rainbow defcustom exists."
  (should (boundp 'flash-rainbow))
  (should (booleanp flash-rainbow)))

(ert-deftest flash-defcustom-highlight-matches-test ()
  "Test that highlight-matches defcustom exists."
  (should (boundp 'flash-highlight-matches))
  (should (booleanp flash-highlight-matches)))

(ert-deftest flash-defcustom-rainbow-shade-test ()
  "Test that rainbow-shade defcustom exists and defaults to 2."
  (should (boundp 'flash-rainbow-shade))
  (should (= 2 flash-rainbow-shade)))

;;; Phase 7 defcustom tests

(ert-deftest flash-defcustom-jumplist-test ()
  "Test that jumplist defcustom exists and defaults to t."
  (should (boundp 'flash-jumplist))
  (should (eq t flash-jumplist)))

(ert-deftest flash-defcustom-search-history-test ()
  "Test that search-history defcustom exists and defaults to nil."
  (should (boundp 'flash-search-history))
  (should (null flash-search-history)))

(ert-deftest flash-defcustom-nohlsearch-test ()
  "Test that nohlsearch defcustom exists and defaults to nil."
  (should (boundp 'flash-nohlsearch))
  (should (null flash-nohlsearch)))

(ert-deftest flash-defcustom-min-pattern-length-test ()
  "Test that min-pattern-length defcustom exists and defaults to 0."
  (should (boundp 'flash-min-pattern-length))
  (should (= 0 flash-min-pattern-length)))

(ert-deftest flash-last-pattern-var-test ()
  "Test that last-pattern variable exists."
  (should (boundp 'flash--last-pattern)))

(ert-deftest flash-jump-continue-exists-test ()
  "Test that flash-jump-continue command exists."
  (should (fboundp 'flash-jump-continue))
  (should (commandp 'flash-jump-continue)))

(ert-deftest flash-multi-window-search-test ()
  "Test search works across multiple windows with different buffers."
  (let ((buf1 (generate-new-buffer "*flash-test-1*"))
        (buf2 (generate-new-buffer "*flash-test-2*"))
        (orig-window (selected-window))
        win1 win2)
    (unwind-protect
        (progn
          ;; Setup two windows with different buffers
          (set-window-buffer orig-window buf1)
          (with-current-buffer buf1
            (insert "hello world hello"))
          (setq win1 orig-window)

          (setq win2 (split-window-right))
          (set-window-buffer win2 buf2)
          (with-current-buffer buf2
            (insert "hello there"))

          ;; Create state with both windows
          (let ((state (flash-state-create (list win1 win2))))
            (setf (flash-state-pattern state) "hello")
            (flash-search state)

            ;; Should find matches in both buffers (2 in buf1, 1 in buf2)
            (should (= 3 (length (flash-state-matches state))))

            ;; Check that markers point to correct buffers
            (dolist (match (flash-state-matches state))
              (let ((marker (flash-match-pos match))
                    (win (flash-match-window match)))
                (should (markerp marker))
                (should (eq (marker-buffer marker) (window-buffer win)))))))

      ;; Cleanup
      (delete-window win2)
      (kill-buffer buf1)
      (kill-buffer buf2))))

(ert-deftest flash-multi-window-highlight-test ()
  "Test highlighting works across multiple windows with different buffers."
  (let ((buf1 (generate-new-buffer "*flash-test-1*"))
        (buf2 (generate-new-buffer "*flash-test-2*"))
        (orig-window (selected-window))
        win1 win2)
    (unwind-protect
        (progn
          ;; Setup two windows with different buffers
          (set-window-buffer orig-window buf1)
          (with-current-buffer buf1
            (insert "test content"))
          (setq win1 orig-window)

          (setq win2 (split-window-right))
          (set-window-buffer win2 buf2)
          (with-current-buffer buf2
            (insert "test data"))

          ;; Create state, search, and highlight
          (let ((state (flash-state-create (list win1 win2))))
            (setf (flash-state-pattern state) "test")
            (flash-search state)
            (flash-label-matches state)

            ;; This should not error - the bug was "Marker points into wrong buffer"
            (flash-highlight-update state)

            ;; Should have overlays in both buffers
            (let ((overlays (flash-state-overlays state)))
              (should (> (length overlays) 0))
              ;; Cleanup overlays
              (flash-highlight-clear state))))

      ;; Cleanup
      (delete-window win2)
      (kill-buffer buf1)
      (kill-buffer buf2))))

(provide 'flash-test)
;;; flash-test.el ends here
