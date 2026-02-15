;;; flash-search.el --- Search functionality for flash -*- lexical-binding: t -*-

;; Copyright (C) 2025 Vadim Pavlov
;; Author: Vadim Pavlov <https://github.com/Prgebish>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Search for pattern matches in visible windows.

;;; Code:

(require 'flash-state)

(defvar flash-case-fold)

(defun flash-search (state)
  "Find all matches for STATE pattern in all windows.
Updates STATE matches field with found matches."
  ;; Release markers from old matches before creating new ones
  (dolist (m (flash-state-matches state))
    (when (markerp (flash-match-pos m))
      (set-marker (flash-match-pos m) nil))
    (when (markerp (flash-match-end-pos m))
      (set-marker (flash-match-end-pos m) nil)))
  (let ((pattern (flash-state-pattern state))
        (windows (flash-state-windows state))
        (case-fold-search flash-case-fold)
        (seen (make-hash-table :test 'equal))
        matches)
    (when (> (length pattern) 0)
      (dolist (win windows)
        (when (window-live-p win)
          (with-selected-window win
            (save-excursion
              (goto-char (window-start win))
              (let ((limit (window-end win t))
                    (buf (window-buffer win)))
                (while (search-forward pattern limit t)
                  (let* ((pos (match-beginning 0))
                         (key (cons buf pos)))
                    ;; Skip duplicate positions from other windows
                    (unless (gethash key seen)
                      (puthash key t seen)
                      (let ((end-pos (match-end 0))
                            (fold (flash--get-fold-at pos)))
                        (push (make-flash-match
                               :pos (copy-marker pos)
                               :end-pos (copy-marker end-pos)
                               :label nil
                               :window win
                               :fold fold)
                              matches)))))))))))
    (setf (flash-state-matches state) (nreverse matches))))

(defun flash--get-fold-at (pos)
  "Return fold line start for POS if it is hidden, otherwise nil.
Works with both text properties and overlays (like hideshow)."
  (when (invisible-p pos)
    ;; Find the overlay or text property that makes this position invisible
    (let ((fold-start nil))
      ;; First check overlays (used by hideshow, outline, etc.)
      (dolist (ov (overlays-at pos))
        (when (overlay-get ov 'invisible)
          (setq fold-start (overlay-start ov))))
      ;; Fallback to text property
      (unless fold-start
        (setq fold-start (previous-single-property-change (1+ pos) 'invisible nil (point-min))))
      ;; Return beginning of line before/at fold start
      (when fold-start
        (save-excursion
          (goto-char fold-start)
          ;; Go to previous line if fold-start is at beginning of line
          (when (bolp)
            (forward-line -1))
          (line-beginning-position))))))

(provide 'flash-search)
;;; flash-search.el ends here
