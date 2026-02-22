;;; flash-state-test.el --- Tests for flash-state -*- lexical-binding: t -*-

;;; Commentary:
;; ERT tests for flash-state module.

;;; Code:

(require 'ert)
(require 'flash-state)

;;; flash-state tests

(ert-deftest flash-state-create-test ()
  "Test state creation with defaults."
  (let ((state (flash-state-create)))
    (should (flash-state-p state))
    (should (string= "" (flash-state-pattern state)))
    (should (null (flash-state-matches state)))
    (should (null (flash-state-overlays state)))
    (should (null (flash-state-target state)))
    (should (eq (selected-window) (flash-state-start-window state)))
    (should (= (point) (flash-state-start-point state)))
    (should (equal (list (selected-window)) (flash-state-windows state)))))

(ert-deftest flash-state-create-with-windows-test ()
  "Test state creation with custom windows."
  (let* ((wins (window-list))
         (state (flash-state-create wins)))
    (should (equal wins (flash-state-windows state)))))

(ert-deftest flash-state-cleanup-test ()
  "Test that cleanup removes overlays."
  (let ((state (flash-state-create)))
    ;; Add some overlays
    (let ((ov1 (make-overlay (point-min) (point-min)))
          (ov2 (make-overlay (point-min) (point-min))))
      (setf (flash-state-overlays state) (list ov1 ov2))
      ;; Verify overlays exist
      (should (overlay-buffer ov1))
      (should (overlay-buffer ov2))
      ;; Cleanup
      (flash-state-cleanup state)
      ;; Verify overlays are deleted
      (should-not (overlay-buffer ov1))
      (should-not (overlay-buffer ov2)))))

;;; flash-match tests

(ert-deftest flash-match-create-test ()
  "Test match creation."
  (let ((match (make-flash-match
                :pos (point-marker)
                :end-pos (point-marker)
                :label ?a
                :window (selected-window)
                :fold nil)))
    (should (flash-match-p match))
    (should (markerp (flash-match-pos match)))
    (should (eq ?a (flash-match-label match)))
    (should (null (flash-match-fold match)))))

(ert-deftest flash-match-with-fold-test ()
  "Test match with fold field."
  (let ((match (make-flash-match
                :pos (point-marker)
                :end-pos (point-marker)
                :label ?b
                :window (selected-window)
                :fold 10)))
    (should (= 10 (flash-match-fold match)))))

(ert-deftest flash-state-cleanup-backdrop-test ()
  "Test that cleanup also deletes backdrop overlays."
  (let ((state (flash-state-create)))
    (let ((ov1 (make-overlay (point-min) (point-min)))
          (ov2 (make-overlay (point-min) (point-min))))
      (setf (flash-state-backdrop-overlays state) (list ov1 ov2))
      ;; Verify overlays exist
      (should (overlay-buffer ov1))
      (should (overlay-buffer ov2))
      ;; Cleanup
      (flash-state-cleanup state)
      ;; Verify backdrop overlays are deleted
      (should-not (overlay-buffer ov1))
      (should-not (overlay-buffer ov2))
      (should (null (flash-state-backdrop-overlays state))))))

(provide 'flash-state-test)
;;; flash-state-test.el ends here
