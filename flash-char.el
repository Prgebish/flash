;;; flash-char.el --- Character motions for flash -*- lexical-binding: t -*-

;; Copyright (C) 2025 Vadim Pavlov
;; Author: Vadim Pavlov <https://github.com/Prgebish>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Enhanced f/t/F/T character motions with visual labels.
;; Like flash.nvim char mode.

;;; Code:

(require 'cl-lib)
(require 'flash-state)
(require 'flash-label)
(require 'flash-highlight)

;;; Customization

(defgroup flash-char nil
  "Character motion settings for flash."
  :group 'flash
  :prefix "flash-char-")

(defcustom flash-char-jump-labels nil
  "When non-nil, show labels for multiple character matches.
When nil, just highlight matches and jump to first one."
  :type 'boolean
  :group 'flash-char)

(defcustom flash-char-multi-line nil
  "When non-nil, search beyond current line.
When nil, only search on current line (like standard Vim behavior)."
  :type 'boolean
  :group 'flash-char)

(defcustom flash-char-reserved-labels ""
  "Characters that should NOT be used as labels in char motions.
These are typically evil/vim editing commands that users want to
execute immediately after f/t/F/T without interference from labels.
Example: \"aisoAISO;,cCxr\" to reserve insert/edit commands."
  :type 'string
  :group 'flash-char)

;;; State Variables

(defvar flash-char--last-motion nil
  "Last char motion type.
One of `find', `find-to', `find-backward', or `find-to-backward'.")

(defvar flash-char--last-char nil
  "Last character searched for.")

(defvar flash-char--last-forward t
  "Non-nil if last search was forward.")

;;; Forward declarations for evil compatibility
(defvar evil-last-find)
(defvar evil-motion-state-map)
(defvar flash-labels)  ; defined in flash.el
(declare-function evil-define-motion "evil-macros")

;;; Helper Functions

(defun flash-char--filtered-labels ()
  "Return `flash-labels' with reserved chars removed."
  (let ((reserved (string-to-list flash-char-reserved-labels)))
    (apply #'string
           (cl-remove-if (lambda (c) (memq c reserved))
                         (string-to-list flash-labels)))))

;;; Core Search Function

(defun flash-char--search (char forward)
  "Search for CHAR in direction FORWARD.
Returns list of positions where CHAR is found (nearest first).
Searches current line only unless `flash-char-multi-line' is non-nil."
  (let ((case-fold-search nil)
        (limit (if forward
                   (if flash-char-multi-line
                       (point-max)
                     (line-end-position))
                 (if flash-char-multi-line
                     (point-min)
                   (line-beginning-position))))
        positions)
    (save-excursion
      ;; Move one char in search direction to avoid matching at point
      (when (and forward (not (eobp)))
        (forward-char 1))
      (if forward
          (while (search-forward (char-to-string char) limit t)
            (push (match-beginning 0) positions))
        (while (search-backward (char-to-string char) limit t)
          (push (point) positions))))
    ;; Return with nearest match first
    (nreverse positions)))

(defun flash-char--create-matches (positions)
  "Create match structs from POSITIONS list."
  (let ((win (selected-window)))
    (mapcar (lambda (pos)
              (make-flash-match
               :pos (copy-marker pos)
               :end-pos (copy-marker (1+ pos))
               :label nil
               :window win
               :fold nil))
            positions)))

;;; Jump with Labels

(defun flash-char--jump-with-labels (matches adjust-fn)
  "Show labels for MATCHES and let user select.
ADJUST-FN is called on final position for t/T motions.
Returns t if jump was made, nil if cancelled."
  (let* ((state (flash-state-create (list (selected-window))))
         jumped)
    (setf (flash-state-matches state) matches)
    (setf (flash-state-pattern state) "")
    ;; Assign labels (excluding reserved chars)
    (let ((flash-labels (flash-char--filtered-labels)))
      (flash-label-matches state))
    ;; Show highlights
    (flash-highlight-update state)
    (unwind-protect
        (let ((char (read-char "Label: ")))
          (cond
           ;; ESC - cancel
           ((= char ?\e)
            (setq jumped nil))
           ;; Label selected
           (t
            (when-let ((match (cl-find (char-to-string char)
                                       (flash-state-matches state)
                                       :key #'flash-match-label
                                       :test #'equal)))
              (goto-char (marker-position (flash-match-pos match)))
              (when adjust-fn (funcall adjust-fn))
              (setq jumped t)))))
      ;; Cleanup
      (flash-state-cleanup state))
    jumped))

(defun flash-char--show-remaining-labels (matches adjust-fn)
  "Show labels for remaining MATCHES after jumping to first.
User can press a label to jump, or any other key to continue.
ADJUST-FN is called on final position for t/T motions."
  (let* ((state (flash-state-create (list (selected-window)))))
    (setf (flash-state-matches state) matches)
    (setf (flash-state-pattern state) "")
    ;; Assign labels (excluding reserved chars)
    (let ((flash-labels (flash-char--filtered-labels)))
      (flash-label-matches state))
    ;; Show highlights
    (flash-highlight-update state)
    (unwind-protect
        (let ((char (read-char)))
          (cond
           ;; ESC or other control chars - just exit
           ((or (= char ?\e) (< char 32))
            nil)
           ;; Label selected - jump to it
           ((when-let ((match (cl-find (char-to-string char)
                                       (flash-state-matches state)
                                       :key #'flash-match-label
                                       :test #'equal)))
              (goto-char (marker-position (flash-match-pos match)))
              (when adjust-fn (funcall adjust-fn))
              t))
           ;; Not a label - put char back for next command
           (t
            (setq unread-command-events (list char)))))
      ;; Cleanup
      (flash-state-cleanup state))))

;;; Motion Implementation

(defun flash-char--do-motion (char forward to-motion &optional update-state)
  "Execute char motion for CHAR in direction FORWARD.
TO-MOTION non-nil means t/T motion (stop before/after char).
UPDATE-STATE non-nil means store this motion for repeat (should be t
for direct f/t/F/T calls, nil for evil reverse repeat).
Returns t if jump was made."
  ;; Store for repeat only if this is a primary motion
  (when update-state
    (setq flash-char--last-char char)
    (setq flash-char--last-forward forward)
    (setq flash-char--last-motion
          (cond ((and forward to-motion) 'find-to)
                ((and forward (not to-motion)) 'find)
                ((and (not forward) to-motion) 'find-to-backward)
                (t 'find-backward)))
    ;; For evil repeat compatibility
    (when (boundp 'evil-last-find)
      (setq evil-last-find
            (list (intern (format "flash-char-%s"
                                  flash-char--last-motion))
                  char forward))))
  ;; Search
  (let ((positions (flash-char--search char forward)))
    (cond
     ;; No matches
     ((null positions)
      (user-error "Can't find '%c'" char)
      nil)
     ;; Single match or labels disabled - jump directly
     ((or (= (length positions) 1)
          (not flash-char-jump-labels))
      (goto-char (car positions))
      (when to-motion
        (if forward (backward-char) (forward-char)))
      t)
     ;; Multiple matches with labels - jump to first, show labels for rest
     (t
      (let* ((first-pos (car positions))
             (rest-positions (cdr positions))
             (adjust-fn (when to-motion
                          (if forward #'backward-char #'forward-char))))
        ;; Jump to first match immediately
        (goto-char first-pos)
        (when to-motion
          (if forward (backward-char) (forward-char)))
        ;; Show labels for remaining matches
        (when rest-positions
          (let ((matches (flash-char--create-matches rest-positions)))
            (flash-char--show-remaining-labels matches adjust-fn)))
        t)))))

;;; Public Motion Commands

;;;###autoload
(defun flash-char-find (count char)
  "Jump forward to CHAR on current line.
COUNT is used only for direction: negative means reverse search
\(for evil repeat compatibility), not for repetition.
With labels enabled, shows labels for multiple matches."
  (interactive "p\ncFind char: ")
  (let* ((cnt (or count 1))
         (forward (>= cnt 0))
         (update-state (>= cnt 0)))  ; don't update state on reverse repeat
    (flash-char--do-motion char forward nil update-state)))

;;;###autoload
(defun flash-char-find-to (count char)
  "Jump forward to before CHAR on current line.
COUNT is used only for direction: negative means reverse search
\(for evil repeat compatibility), not for repetition.
With labels enabled, shows labels for multiple matches."
  (interactive "p\ncFind char (to): ")
  (let* ((cnt (or count 1))
         (forward (>= cnt 0))
         (update-state (>= cnt 0)))
    (flash-char--do-motion char forward t update-state)))

;;;###autoload
(defun flash-char-find-backward (count char)
  "Jump backward to CHAR on current line.
COUNT is used only for direction: negative means forward search
\(for evil repeat compatibility), not for repetition.
With labels enabled, shows labels for multiple matches."
  (interactive "p\ncFind char backward: ")
  (let* ((cnt (or count 1))
         (forward (< cnt 0))
         (update-state (>= cnt 0)))
    (flash-char--do-motion char forward nil update-state)))

;;;###autoload
(defun flash-char-find-to-backward (count char)
  "Jump backward to after CHAR on current line.
COUNT is used only for direction: negative means forward search
\(for evil repeat compatibility), not for repetition.
With labels enabled, shows labels for multiple matches."
  (interactive "p\ncFind char backward (to): ")
  (let* ((cnt (or count 1))
         (forward (< cnt 0))
         (update-state (>= cnt 0)))
    (flash-char--do-motion char forward t update-state)))

;;; Repeat Commands

;;;###autoload
(defun flash-char-repeat ()
  "Repeat last char motion in same direction."
  (interactive)
  (unless flash-char--last-char
    (user-error "No previous char search"))
  (let ((to-motion (memq flash-char--last-motion
                         '(find-to find-to-backward))))
    (flash-char--do-motion
     flash-char--last-char
     flash-char--last-forward
     to-motion
     nil)))  ; don't update state on repeat

;;;###autoload
(defun flash-char-repeat-reverse ()
  "Repeat last char motion in opposite direction."
  (interactive)
  (unless flash-char--last-char
    (user-error "No previous char search"))
  (let ((to-motion (memq flash-char--last-motion
                         '(find-to find-to-backward))))
    (flash-char--do-motion
     flash-char--last-char
     (not flash-char--last-forward)
     to-motion
     nil)))  ; don't update state on reverse repeat

;;; Evil Integration

(defun flash-char-setup-evil-keys ()
  "Setup evil keybindings for flash char motions.
Replaces standard f/t/F/T with flash versions."
  (when (featurep 'evil)
    (define-key evil-motion-state-map "f" #'flash-char-find)
    (define-key evil-motion-state-map "t" #'flash-char-find-to)
    (define-key evil-motion-state-map "F" #'flash-char-find-backward)
    (define-key evil-motion-state-map "T" #'flash-char-find-to-backward)
    (define-key evil-motion-state-map ";" #'flash-char-repeat)
    (define-key evil-motion-state-map "," #'flash-char-repeat-reverse)))

(provide 'flash-char)
;;; flash-char.el ends here
