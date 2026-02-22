;;; flash-highlight-test.el --- Tests for flash-highlight -*- lexical-binding: t -*-

;;; Commentary:
;; ERT tests for flash-highlight module.

;;; Code:

(require 'ert)
(require 'flash-state)
(require 'flash-highlight)

;; Variables dynamically bound in tests.
(defvar flash-backdrop)
(defvar flash-label-position)
(defvar flash-rainbow)
(defvar flash-rainbow-shade)

(ert-deftest flash-highlight-clear-test ()
  "Test that highlight-clear removes all overlays."
  (with-temp-buffer
    (insert "test content")
    (let ((state (flash-state-create)))
      ;; Add some overlays manually
      (let ((ov1 (make-overlay 1 5))
            (ov2 (make-overlay 6 10)))
        (setf (flash-state-overlays state) (list ov1 ov2))
        (should (= 2 (length (flash-state-overlays state))))
        ;; Clear
        (flash-highlight-clear state)
        ;; Should be empty
        (should (null (flash-state-overlays state)))
        ;; Overlays should be deleted
        (should-not (overlay-buffer ov1))
        (should-not (overlay-buffer ov2))))))

(ert-deftest flash-highlight-update-creates-overlays-test ()
  "Test that highlight-update creates overlays for matches."
  (with-temp-buffer
    (insert "foo bar foo")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window))))
          (flash-backdrop nil))  ; disable backdrop for simpler test
      ;; Add a match manually
      (setf (flash-state-matches state)
            (list (make-flash-match
                   :pos (copy-marker 1)
                   :end-pos (copy-marker 4)
                   :label "a"
                   :window (selected-window)
                   :fold nil)))
      ;; Update highlights
      (flash-highlight-update state)
      ;; Should have overlays (match + label)
      (should (>= (length (flash-state-overlays state)) 2)))))

(ert-deftest flash-highlight-backdrop-test ()
  "Test that backdrop overlay is created when enabled."
  (with-temp-buffer
    (insert "test content here")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window))))
          (flash-backdrop t))
      (setf (flash-state-matches state) nil)
      (flash-highlight-update state)
      ;; Backdrop should be in backdrop-overlays, not overlays
      (should (>= (length (flash-state-backdrop-overlays state)) 1))
      ;; Check it has backdrop face
      (let ((ov (car (flash-state-backdrop-overlays state))))
        (should (eq 'flash-backdrop (overlay-get ov 'face)))))))

(ert-deftest flash-highlight-no-backdrop-test ()
  "Test that no backdrop when disabled."
  (with-temp-buffer
    (insert "test content")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window))))
          (flash-backdrop nil))
      (setf (flash-state-matches state) nil)
      (flash-highlight-update state)
      ;; Should have no overlays at all
      (should (null (flash-state-overlays state)))
      (should (null (flash-state-backdrop-overlays state))))))

(ert-deftest flash-highlight-label-test ()
  "Test that label is displayed as after-string (default position)."
  (with-temp-buffer
    (insert "foo bar")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window))))
          (flash-backdrop nil)
          (flash-label-position 'after))
      (setf (flash-state-matches state)
            (list (make-flash-match
                   :pos (copy-marker 1)
                   :end-pos (copy-marker 4)
                   :label "x"
                   :window (selected-window)
                   :fold nil)))
      (flash-highlight-update state)
      ;; Find label overlay
      (let ((label-ov (seq-find (lambda (ov)
                                  (overlay-get ov 'after-string))
                                (flash-state-overlays state))))
        (should label-ov)
        (should (string= "x" (overlay-get label-ov 'after-string)))))))

(ert-deftest flash-highlight-match-without-label-test ()
  "Test that match without label still gets highlight."
  (with-temp-buffer
    (insert "foo bar")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window))))
          (flash-backdrop nil))
      (setf (flash-state-matches state)
            (list (make-flash-match
                   :pos (copy-marker 1)
                   :end-pos (copy-marker 4)
                   :label nil  ; no label
                   :window (selected-window)
                   :fold nil)))
      (flash-highlight-update state)
      ;; Should have match overlay
      (should (= 1 (length (flash-state-overlays state))))
      (should (eq 'flash-match
                  (overlay-get (car (flash-state-overlays state)) 'face))))))

(ert-deftest flash-highlight-rainbow-disabled-test ()
  "Test that default face is used when rainbow disabled."
  (let ((flash-rainbow nil))
    (should (eq 'flash-label (flash--get-label-face 0)))
    (should (eq 'flash-label (flash--get-label-face 1)))
    (should (eq 'flash-label (flash--get-label-face 5)))))

(ert-deftest flash-highlight-rainbow-enabled-test ()
  "Test that rainbow faces return plists with correct colors.
Default shade 5: bg from shade-500, fg from shade-950."
  (let ((flash-rainbow t)
        (flash-rainbow-shade 5))
    ;; Red: bg=red-500, fg=red-950
    (let ((face (flash--get-label-face 0)))
      (should (listp face))
      (should (string= "#ef4444" (plist-get face :background)))
      (should (string= "#450a0a" (plist-get face :foreground))))
    ;; Amber: bg=amber-500, fg=amber-950
    (let ((face (flash--get-label-face 1)))
      (should (string= "#f59e0b" (plist-get face :background)))
      (should (string= "#451a03" (plist-get face :foreground))))
    ;; Should cycle after 10
    (should (equal (flash--get-label-face 0)
                   (flash--get-label-face 10)))))

(ert-deftest flash-highlight-rainbow-shade-test ()
  "Test that different shades produce correct bg/fg colors."
  (let ((flash-rainbow t))
    ;; Shade 2: pastel bg (200), dark fg (900)
    (let ((flash-rainbow-shade 2))
      (let ((face (flash--get-label-face 0)))
        (should (string= "#fecaca" (plist-get face :background)))   ; red-200
        (should (string= "#7f1d1d" (plist-get face :foreground))))) ; red-900
    ;; Shade 5: saturated bg (500), very dark fg (950)
    (let ((flash-rainbow-shade 5))
      (let ((face (flash--get-label-face 0)))
        (should (string= "#ef4444" (plist-get face :background)))   ; red-500
        (should (string= "#450a0a" (plist-get face :foreground))))) ; red-950
    ;; Shade 7: dark bg (700), light fg (50)
    (let ((flash-rainbow-shade 7))
      (let ((face (flash--get-label-face 0)))
        (should (string= "#b91c1c" (plist-get face :background)))   ; red-700
        (should (string= "#fef2f2" (plist-get face :foreground)))))))  ; red-50

(ert-deftest flash-highlight-rainbow-labels-visual-test ()
  "Test that rainbow labels are applied to overlays."
  (with-temp-buffer
    (insert "foo bar foo baz foo")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window))))
          (flash-backdrop nil)
          (flash-rainbow t))
      ;; Add multiple matches with labels
      (setf (flash-state-matches state)
            (list (make-flash-match
                   :pos (copy-marker 1) :end-pos (copy-marker 4)
                   :label "a" :window (selected-window) :fold nil)
                  (make-flash-match
                   :pos (copy-marker 9) :end-pos (copy-marker 12)
                   :label "s" :window (selected-window) :fold nil)
                  (make-flash-match
                   :pos (copy-marker 17) :end-pos (copy-marker 20)
                   :label "d" :window (selected-window) :fold nil)))
      (flash-highlight-update state)
      ;; Should have 6 overlays (3 matches + 3 labels)
      (should (= 6 (length (flash-state-overlays state))))
      ;; Check label overlays exist (display property for 'overlay position)
      (let ((label-ovs (seq-filter (lambda (ov) (overlay-get ov 'display))
                                   (flash-state-overlays state))))
        (should (= 3 (length label-ovs)))))))

;;; Label Position Tests

(ert-deftest flash-highlight-label-position-before-test ()
  "Test label position 'before'."
  (with-temp-buffer
    (insert "foo bar")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window))))
          (flash-backdrop nil)
          (flash-label-position 'before))
      (setf (flash-state-matches state)
            (list (make-flash-match
                   :pos (copy-marker 1)
                   :end-pos (copy-marker 4)
                   :label "x"
                   :window (selected-window)
                   :fold nil)))
      (flash-highlight-update state)
      ;; Find label overlay with before-string
      (let ((label-ov (seq-find (lambda (ov)
                                  (overlay-get ov 'before-string))
                                (flash-state-overlays state))))
        (should label-ov)
        (should (string= "x" (overlay-get label-ov 'before-string)))))))

(ert-deftest flash-highlight-label-position-overlay-test ()
  "Test label position 'overlay' (replaces first char)."
  (with-temp-buffer
    (insert "foo bar")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window))))
          (flash-backdrop nil)
          (flash-label-position 'overlay))
      (setf (flash-state-matches state)
            (list (make-flash-match
                   :pos (copy-marker 1)
                   :end-pos (copy-marker 4)
                   :label "x"
                   :window (selected-window)
                   :fold nil)))
      (flash-highlight-update state)
      ;; Find label overlay with display property
      (let ((label-ov (seq-find (lambda (ov)
                                  (overlay-get ov 'display))
                                (flash-state-overlays state))))
        (should label-ov)
        (should (string= "x" (overlay-get label-ov 'display)))
        ;; Overlay should cover exactly 1 character
        (should (= 1 (- (overlay-end label-ov) (overlay-start label-ov))))))))

(ert-deftest flash-highlight-label-position-eol-test ()
  "Test label position 'eol' (end of line)."
  (with-temp-buffer
    (insert "foo bar\nbaz")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window))))
          (flash-backdrop nil)
          (flash-label-position 'eol))
      (setf (flash-state-matches state)
            (list (make-flash-match
                   :pos (copy-marker 1)
                   :end-pos (copy-marker 4)
                   :label "x"
                   :window (selected-window)
                   :fold nil)))
      (flash-highlight-update state)
      ;; Find label overlay at end of line
      (let ((label-ov (seq-find (lambda (ov)
                                  (and (overlay-get ov 'after-string)
                                       (= (overlay-start ov) 8))) ; EOL position
                                (flash-state-overlays state))))
        (should label-ov)
        ;; after-string should contain label (with space prefix)
        (should (string-match-p "x" (overlay-get label-ov 'after-string)))))))

(ert-deftest flash-highlight-label-position-pre-overlay-mid-line-test ()
  "Test pre-overlay replaces char before match (mid-line)."
  (with-temp-buffer
    (insert "foo bar")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window))))
          (flash-backdrop nil)
          (flash-label-position 'pre-overlay))
      ;; Match "bar" at position 5
      (setf (flash-state-matches state)
            (list (make-flash-match
                   :pos (copy-marker 5)
                   :end-pos (copy-marker 8)
                   :label "x"
                   :window (selected-window)
                   :fold nil)))
      (flash-highlight-update state)
      (let ((label-ov (seq-find (lambda (ov) (overlay-get ov 'display))
                                (flash-state-overlays state))))
        (should label-ov)
        (should (string= "x" (overlay-get label-ov 'display)))
        ;; Should cover char before match (position 4 = space)
        (should (= 4 (overlay-start label-ov)))
        (should (= 5 (overlay-end label-ov)))))))

(ert-deftest flash-highlight-label-position-pre-overlay-bol-test ()
  "Test pre-overlay falls back to first char at beginning of line."
  (with-temp-buffer
    (insert "foo\nbar")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window))))
          (flash-backdrop nil)
          (flash-label-position 'pre-overlay))
      ;; Match "bar" at position 5 (beginning of second line)
      (setf (flash-state-matches state)
            (list (make-flash-match
                   :pos (copy-marker 5)
                   :end-pos (copy-marker 8)
                   :label "x"
                   :window (selected-window)
                   :fold nil)))
      (flash-highlight-update state)
      (let ((label-ov (seq-find (lambda (ov) (overlay-get ov 'display))
                                (flash-state-overlays state))))
        (should label-ov)
        ;; Fallback: overlay on first char of match
        (should (= 5 (overlay-start label-ov)))
        (should (= 6 (overlay-end label-ov)))))))

(ert-deftest flash-highlight-label-position-pre-overlay-bob-test ()
  "Test pre-overlay falls back to first char at beginning of buffer."
  (with-temp-buffer
    (insert "foo bar")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window))))
          (flash-backdrop nil)
          (flash-label-position 'pre-overlay))
      ;; Match "foo" at position 1 (beginning of buffer)
      (setf (flash-state-matches state)
            (list (make-flash-match
                   :pos (copy-marker 1)
                   :end-pos (copy-marker 4)
                   :label "x"
                   :window (selected-window)
                   :fold nil)))
      (flash-highlight-update state)
      (let ((label-ov (seq-find (lambda (ov) (overlay-get ov 'display))
                                (flash-state-overlays state))))
        (should label-ov)
        ;; Fallback: overlay on first char of match
        (should (= 1 (overlay-start label-ov)))
        (should (= 2 (overlay-end label-ov)))))))

(ert-deftest flash-highlight-label-position-defcustom-test ()
  "Test that label-position defcustom exists."
  (should (boundp 'flash-label-position)))

(ert-deftest flash-highlight-backdrop-reuse-test ()
  "Test that backdrop overlays are reused across updates."
  (with-temp-buffer
    (insert "foo bar foo")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window))))
          (flash-backdrop t))
      (setf (flash-state-matches state) nil)
      ;; First update creates backdrop
      (flash-highlight-update state)
      (let ((backdrop-1 (copy-sequence (flash-state-backdrop-overlays state))))
        (should (>= (length backdrop-1) 1))
        ;; Second update should reuse same overlay objects
        (flash-highlight-update state)
        (should (equal backdrop-1 (flash-state-backdrop-overlays state)))))))

(ert-deftest flash-highlight-clear-all-test ()
  "Test that clear-all removes both match/label and backdrop overlays."
  (with-temp-buffer
    (insert "foo bar foo")
    (goto-char (point-min))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((state (flash-state-create (list (selected-window))))
          (flash-backdrop t))
      (setf (flash-state-matches state)
            (list (make-flash-match
                   :pos (copy-marker 1)
                   :end-pos (copy-marker 4)
                   :label "a"
                   :window (selected-window)
                   :fold nil)))
      (flash-highlight-update state)
      ;; Should have both types of overlays
      (should (>= (length (flash-state-overlays state)) 1))
      (should (>= (length (flash-state-backdrop-overlays state)) 1))
      ;; Clear all
      (flash-highlight-clear-all state)
      (should (null (flash-state-overlays state)))
      (should (null (flash-state-backdrop-overlays state))))))

(provide 'flash-highlight-test)
;;; flash-highlight-test.el ends here
