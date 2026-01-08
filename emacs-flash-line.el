;;; emacs-flash-line.el --- Line mode for emacs-flash -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: chestnykh
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;; Jump to any visible line without typing a search pattern.
;; Similar to avy-goto-line but uses flash infrastructure.

;;; Code:

(require 'cl-lib)
(require 'emacs-flash-state)
(require 'emacs-flash-label)
(require 'emacs-flash-highlight)
(require 'emacs-flash-jump)

;;; Configuration (from emacs-flash.el)

(defvar emacs-flash-multi-window)
(defvar emacs-flash-labels)

;;; Line Mode Functions

;;;###autoload
(defun emacs-flash-line-jump ()
  "Jump to the beginning of any visible line.
Shows labels at the start of each line in visible windows.
Press a label key to jump to that line."
  (interactive)
  (let ((windows (if (bound-and-true-p emacs-flash-multi-window)
                     (window-list nil 'no-minibuf)
                   (list (selected-window)))))
    (let ((state (emacs-flash-state-create windows)))
      (setf (emacs-flash-state-start-window state) (selected-window))
      (setf (emacs-flash-state-start-point state) (point))
      (unwind-protect
          (emacs-flash-line--run state)
        (emacs-flash-highlight-clear state)
        (emacs-flash-state-cleanup state)))))

(defun emacs-flash-line--run (state)
  "Run line mode with STATE."
  ;; Collect all visible line starts as matches
  (emacs-flash-line--collect-lines state)
  ;; Assign labels
  (emacs-flash-label-matches state)
  ;; Show highlights
  (emacs-flash-highlight-update state)
  ;; Read input and jump
  (emacs-flash-line--input-loop state))

(defun emacs-flash-line--collect-lines (state)
  "Collect line start positions in all windows of STATE."
  (let ((matches nil))
    (dolist (win (emacs-flash-state-windows state))
      (when (window-live-p win)
        (with-current-buffer (window-buffer win)
          (let ((start (window-start win))
                (end (window-end win t)))
            (save-excursion
              (goto-char start)
              (while (< (point) end)
                (let ((line-start (line-beginning-position)))
                  ;; Only add if line is visible (not in fold)
                  (unless (invisible-p line-start)
                    (push (make-emacs-flash-match
                           :pos (copy-marker line-start)
                           :end-pos (copy-marker (1+ line-start))
                           :label nil
                           :window win
                           :fold nil)
                          matches)))
                (forward-line 1)))))))
    ;; Store matches sorted by position
    (setf (emacs-flash-state-matches state) (nreverse matches))))

(defun emacs-flash-line--input-loop (state)
  "Input loop for line mode STATE.
Returns t if jump was made, nil if cancelled."
  (catch 'emacs-flash-line-done
    (while t
      (redisplay t)
      (let* ((prefix (emacs-flash-state-label-prefix state))
             (prompt (if prefix
                         (format "Line [%s]: " prefix)
                       "Line: "))
             (char (read-char prompt))
             (char-str (char-to-string char)))
        (cond
         ;; Escape - cancel (or clear prefix)
         ((= char ?\e)
          (if prefix
              (progn
                (setf (emacs-flash-state-label-prefix state) nil)
                ;; Re-show all labels
                (emacs-flash-highlight-update state))
            (emacs-flash-return-to-start state)
            (throw 'emacs-flash-line-done nil)))

         ;; Check if it completes a label
         ((let ((full-label (concat (or prefix "") char-str)))
            (emacs-flash-jump-to-label state full-label))
          (throw 'emacs-flash-line-done t))

         ;; Check if it's a valid label prefix
         ((emacs-flash-line--valid-prefix-p state char-str)
          (setf (emacs-flash-state-label-prefix state) char-str)
          ;; Update display to show filtered labels
          (emacs-flash-highlight-update state))

         ;; Invalid input
         (t (beep)))))))

(defun emacs-flash-line--valid-prefix-p (state prefix)
  "Return t if PREFIX is the start of any multi-char label in STATE."
  (cl-some (lambda (match)
             (let ((label (emacs-flash-match-label match)))
               (and label
                    (> (length label) 1)
                    (string-prefix-p prefix label))))
           (emacs-flash-state-matches state)))

(provide 'emacs-flash-line)
;;; emacs-flash-line.el ends here
