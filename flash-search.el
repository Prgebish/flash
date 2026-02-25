;;; flash-search.el --- Search functionality for flash -*- lexical-binding: t -*-

;; Copyright (C) 2025 Vadim Pavlov
;; Author: Vadim Pavlov <https://github.com/Prgebish>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Search for pattern matches in visible windows.

;;; Code:

(require 'flash-state)

(defvar flash-case-fold t)
(defvar flash-search-folds nil)

(defun flash-search (state)
  "Find all matches for STATE pattern in all windows.
Updates STATE matches field with found matches."
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
                         (key (cons win pos)))
                    ;; Skip duplicate positions within the same window
                    (unless (gethash key seen)
                      (puthash key t seen)
                      (let* ((end-pos (match-end 0))
                             (hidden (invisible-p pos))
                             (fold (when (and hidden flash-search-folds)
                                     (flash--get-fold-at pos))))
                        ;; Skip invisible matches unless fold search is enabled
                        (unless (and hidden (not flash-search-folds))
                          (push (make-flash-match
                                 :pos pos
                                 :end-pos end-pos
                                 :buffer buf
                                 :label nil
                                 :window win
                                 :fold fold)
                                matches))))))))))))
    (setf (flash-state-matches state) (nreverse matches))
    ;; Labels are invalidated after each search and rebuilt separately.
    (setf (flash-state-label-index state) nil)))

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
