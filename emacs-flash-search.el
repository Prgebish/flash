;;; emacs-flash-search.el --- Search functionality for emacs-flash -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: chestnykh
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;; Search for pattern matches in visible windows.

;;; Code:

(require 'emacs-flash-state)

(defvar emacs-flash-case-fold)

(defun emacs-flash-search (state)
  "Find all matches for STATE pattern in all windows.
Updates STATE matches field with found matches."
  ;; Release markers from old matches before creating new ones
  (dolist (m (emacs-flash-state-matches state))
    (when (markerp (emacs-flash-match-pos m))
      (set-marker (emacs-flash-match-pos m) nil))
    (when (markerp (emacs-flash-match-end-pos m))
      (set-marker (emacs-flash-match-end-pos m) nil)))
  (let ((pattern (emacs-flash-state-pattern state))
        (windows (emacs-flash-state-windows state))
        (case-fold-search emacs-flash-case-fold)
        matches)
    (when (> (length pattern) 0)
      (dolist (win windows)
        (with-selected-window win
          (save-excursion
            (goto-char (window-start win))
            (let ((limit (window-end win t)))
              (while (search-forward pattern limit t)
                (let* ((pos (match-beginning 0))
                       (end-pos (match-end 0))
                       (fold (emacs-flash--get-fold-at pos)))
                  (push (make-emacs-flash-match
                         :pos (copy-marker pos)
                         :end-pos (copy-marker end-pos)
                         :label nil
                         :window win
                         :fold fold)
                        matches))))))))
    (setf (emacs-flash-state-matches state) (nreverse matches))))

(defun emacs-flash--get-fold-at (pos)
  "Return fold line start position if POS is in invisible/folded region, nil otherwise.
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

(provide 'emacs-flash-search)
;;; emacs-flash-search.el ends here
