;; flash.el — navigation with search labels

(defun flash-jump ()
  "Start flash jump session."
  (interactive)
  (let* ((windows (window-list nil 'no-minibuf))
         (state (flash-state-create windows)))
    (unwind-protect
        (flash--loop state)
      (flash-state-cleanup state))))

(defun flash-search (state)
  "Find all matches for pattern in visible windows."
  (let ((pattern (flash-state-pattern state))
        (case-fold-search flash-case-fold))
    (dolist (win (flash-state-windows state))
      (when (window-live-p win)
        (with-selected-window win
          (save-excursion
            (goto-char (window-start win))
            (while (search-forward pattern (window-end win t) t)
              (push (make-flash-match
                     :pos (copy-marker (match-beginning 0))
                     :end-pos (copy-marker (match-end 0))
                     :window win)
                    (flash-state-matches state)))))))))

(defun flash-highlight-update (state)
  "Update overlays for backdrop, matches, and labels."
  (flash-highlight-clear state)
  (when flash-backdrop
    (flash--highlight-backdrop state))
  (dolist (match (flash-state-matches state))
    (flash--highlight-match state match)))

(defun flash-label-matches (state)
  "Assign labels to matches, sorted by distance from cursor.
Labels that could continue the search pattern are skipped."
  (let* ((labels (flash--available-labels state))
         (sorted (flash--sort-by-distance
                  (flash-state-matches state)
                  (point)))
         (index 0))
    (dolist (match sorted)
      (when (< index (length labels))
        (setf (flash-match-label match)
              (char-to-string (aref labels index)))
        (cl-incf index)))))

(defun flash--sort-by-distance (matches origin)
  "Sort MATCHES by distance from ORIGIN."
  (sort (copy-sequence matches)
        (lambda (a b)
          (< (abs (- (flash-match-pos a) origin))
             (abs (- (flash-match-pos b) origin))))))

(provide 'flash-demo)
