;;; emacs-flash.el --- Flash-style navigation for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: chestnykh
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, navigation
;; URL: https://github.com/chestnykh/emacs-flash

;;; Commentary:
;; Fast navigation using labels assigned to search matches.
;; Similar to flash.nvim for Neovim.
;;
;; Features:
;; - Incremental search with instant feedback
;; - Smart labels that skip conflicting characters
;; - Multi-window search support
;; - Autojump when single match
;; - Fold-aware navigation
;;
;; Usage:
;;   M-x emacs-flash-jump
;;
;; Or bind to a key:
;;   (global-set-key (kbd "s-j") #'emacs-flash-jump)

;;; Code:

(require 'cl-lib)
(require 'emacs-flash-state)
(require 'emacs-flash-search)
(require 'emacs-flash-label)
(require 'emacs-flash-highlight)
(require 'emacs-flash-jump)

;;; Customization

(defgroup emacs-flash nil
  "Flash-style navigation."
  :group 'convenience
  :prefix "emacs-flash-")

(defcustom emacs-flash-labels "asdfjkl;ghqwertyuiopzxcvbnm"
  "Characters to use as jump labels, ordered by priority.
Earlier characters are assigned to closer matches."
  :type 'string
  :group 'emacs-flash)

(defcustom emacs-flash-multi-char-labels nil
  "When non-nil, use multi-character labels (aa, as, ad...) when needed.
Multi-char labels are used when matches exceed available single-char labels.
When nil, only single-character labels are used and excess matches are unlabeled."
  :type 'boolean
  :group 'emacs-flash)

(defcustom emacs-flash-multi-window t
  "When non-nil, search in all visible windows."
  :type 'boolean
  :group 'emacs-flash)

(defcustom emacs-flash-autojump t
  "When non-nil, automatically jump when there is only one match."
  :type 'boolean
  :group 'emacs-flash)

(defcustom emacs-flash-backdrop t
  "When non-nil, dim non-matching text with backdrop effect."
  :type 'boolean
  :group 'emacs-flash)

(defcustom emacs-flash-case-fold t
  "When non-nil, ignore case when searching."
  :type 'boolean
  :group 'emacs-flash)

(defcustom emacs-flash-rainbow nil
  "When non-nil, use different colors for each label.
Makes labels more visually distinct."
  :type 'boolean
  :group 'emacs-flash)

(defcustom emacs-flash-highlight-matches t
  "When non-nil, highlight matched text.
When nil, only labels are shown, keeping original syntax highlighting."
  :type 'boolean
  :group 'emacs-flash)

(defcustom emacs-flash-label-position 'after
  "Where to display the jump label relative to the match.
- `after': Label appears after the match (default, like flash.nvim)
- `before': Label appears before the match
- `overlay': Label replaces the first character of the match
- `eol': Label appears at end of line"
  :type '(choice (const :tag "After match" after)
                 (const :tag "Before match" before)
                 (const :tag "Over first char" overlay)
                 (const :tag "End of line" eol))
  :group 'emacs-flash)

(defcustom emacs-flash-jump-position 'start
  "Where to place cursor after jumping to a match.
- `start': Cursor at the beginning of the match (default)
- `end': Cursor at the end of the match"
  :type '(choice (const :tag "Start of match" start)
                 (const :tag "End of match" end))
  :group 'emacs-flash)

(defcustom emacs-flash-jumplist t
  "When non-nil, save position to mark ring before jumping.
This allows returning to the previous position with `C-u C-SPC'
or `C-x C-SPC' (global mark), or with evil's `C-o'."
  :type 'boolean
  :group 'emacs-flash)

(defcustom emacs-flash-search-history nil
  "When non-nil, add search pattern to Emacs search history.
The pattern will appear in `isearch' history (M-p/M-n)."
  :type 'boolean
  :group 'emacs-flash)

(defcustom emacs-flash-nohlsearch nil
  "When non-nil, clear search highlighting after jump.
Works with both isearch and evil-search highlighting."
  :type 'boolean
  :group 'emacs-flash)

(defcustom emacs-flash-min-pattern-length 0
  "Minimum pattern length to show jump labels.
Labels won't appear until the pattern reaches this length.
Set to 0 to always show labels (default)."
  :type 'integer
  :group 'emacs-flash)

;;; State for continue functionality

(defvar emacs-flash--last-pattern nil
  "Last search pattern used in flash jump.")

;;; Forward declarations for evil
(defvar evil-ex-search-history)
(defvar evil-search-module)
(defvar evil-search-forward-history)

(defun emacs-flash--add-to-evil-search-history (pattern)
  "Add PATTERN to evil search history.
Handles both `evil-search' and `isearch' modules."
  (cond
   ;; evil-search module uses evil-ex-search-history
   ((and (boundp 'evil-search-module)
         (eq evil-search-module 'evil-search)
         (boundp 'evil-ex-search-history))
    (unless (equal pattern (car evil-ex-search-history))
      (push pattern evil-ex-search-history)))
   ;; isearch module uses evil-search-forward-history
   ((boundp 'evil-search-forward-history)
    (unless (equal pattern (car evil-search-forward-history))
      (push pattern evil-search-forward-history)))))

;;; Main Command

;;;###autoload
(defun emacs-flash-jump ()
  "Start flash jump session.
Type characters to search, then press a label to jump.
Press RET to jump to first match, ESC to cancel."
  (interactive)
  (let ((windows (if emacs-flash-multi-window
                     (window-list nil 'no-minibuf)
                   (list (selected-window)))))
    (let ((state (emacs-flash-state-create windows)))
      (setf (emacs-flash-state-start-window state) (selected-window))
      (setf (emacs-flash-state-start-point state) (point))
      (unwind-protect
          (emacs-flash--loop state)
        (emacs-flash-highlight-clear state)
        (emacs-flash-state-cleanup state)))))

;;; Main Loop

(defun emacs-flash--loop (state)
  "Main input loop for STATE.
Returns t if jump was made, nil if cancelled."
  (catch 'emacs-flash-done
    (while t
      ;; Update search results
      (emacs-flash-search state)

      ;; Assign labels (respecting min-pattern-length)
      (if (>= (length (emacs-flash-state-pattern state))
              emacs-flash-min-pattern-length)
          (emacs-flash-label-matches state)
        ;; Clear labels if pattern too short
        (dolist (match (emacs-flash-state-matches state))
          (setf (emacs-flash-match-label match) nil)))

      ;; Update display
      (emacs-flash-highlight-update state)

      ;; Autojump if single match (and pattern long enough)
      (when (and emacs-flash-autojump
                 (= (length (emacs-flash-state-matches state)) 1)
                 (>= (length (emacs-flash-state-pattern state))
                     emacs-flash-min-pattern-length))
        (emacs-flash--do-jump state)
        (throw 'emacs-flash-done t))

      ;; Read input
      (redisplay t)  ; Force display update before reading
      (let* ((pattern (emacs-flash-state-pattern state))
             (prefix (emacs-flash-state-label-prefix state))
             (match-count (length (emacs-flash-state-matches state)))
             (prompt (emacs-flash--format-prompt pattern match-count prefix))
             (char (read-char prompt))
             (char-str (char-to-string char)))
        (cond
         ;; Escape - cancel
         ((= char ?\e)
          (emacs-flash-return-to-start state)
          (throw 'emacs-flash-done nil))

         ;; Enter - jump to first match
         ((= char ?\r)
          (when (emacs-flash-state-matches state)
            (emacs-flash--do-jump state))
          (throw 'emacs-flash-done t))

         ;; Backspace - delete last char or clear prefix
         ((or (= char ?\C-?) (= char ?\C-h) (= char 127))
          (cond
           (prefix
            ;; Clear prefix first
            (setf (emacs-flash-state-label-prefix state) nil))
           ((> (length pattern) 0)
            ;; Then delete from pattern
            (setf (emacs-flash-state-pattern state)
                  (substring pattern 0 -1)))))

         ;; Check if it completes a label (with current prefix)
         ((let ((full-label (concat (or prefix "") char-str)))
            (emacs-flash-jump-to-label state full-label))
          (emacs-flash--save-pattern state)
          (throw 'emacs-flash-done t))

         ;; Check if it's a valid label prefix (for multi-char labels)
         ((emacs-flash--valid-label-prefix-p state char-str)
          (setf (emacs-flash-state-label-prefix state) char-str))

         ;; Add to pattern (only if no prefix active)
         ((not prefix)
          (setf (emacs-flash-state-pattern state)
                (concat pattern char-str)))

         ;; Invalid input with prefix - ignore or beep
         (t (beep)))))))

(defun emacs-flash--do-jump (state)
  "Perform jump to first match in STATE.
Also saves pattern and adds to search history."
  (emacs-flash-jump-to-first state)
  (emacs-flash--save-pattern state))

(defun emacs-flash--save-pattern (state)
  "Save pattern from STATE for continue and search history."
  (let ((pattern (emacs-flash-state-pattern state)))
    (when (> (length pattern) 0)
      ;; Save for continue
      (setq emacs-flash--last-pattern pattern)
      ;; Add to search history
      (when emacs-flash-search-history
        ;; Emacs isearch history
        (isearch-update-ring pattern)
        ;; Evil search history (if available)
        (when (featurep 'evil)
          (emacs-flash--add-to-evil-search-history pattern))))))

(defun emacs-flash--format-prompt (pattern match-count &optional prefix)
  "Format prompt string showing PATTERN, MATCH-COUNT, and optional PREFIX."
  (let ((base (if (string-empty-p pattern)
                  "Flash: "
                (format "Flash [%s] (%d): " pattern match-count))))
    (if prefix
        (concat base prefix)
      base)))

(defun emacs-flash--valid-label-prefix-p (state prefix)
  "Return t if PREFIX is the start of any multi-char label in STATE."
  (cl-some (lambda (match)
             (let ((label (emacs-flash-match-label match)))
               (and label
                    (> (length label) 1)
                    (string-prefix-p prefix label))))
           (emacs-flash-state-matches state)))

;;;###autoload
(defun emacs-flash-jump-continue ()
  "Continue flash jump with the last search pattern.
If no previous pattern exists, starts a new search."
  (interactive)
  (let ((windows (if emacs-flash-multi-window
                     (window-list nil 'no-minibuf)
                   (list (selected-window)))))
    (let ((state (emacs-flash-state-create windows)))
      (setf (emacs-flash-state-start-window state) (selected-window))
      (setf (emacs-flash-state-start-point state) (point))
      ;; Set initial pattern from last search
      (when emacs-flash--last-pattern
        (setf (emacs-flash-state-pattern state) emacs-flash--last-pattern))
      (unwind-protect
          (emacs-flash--loop state)
        (emacs-flash-highlight-clear state)
        (emacs-flash-state-cleanup state)))))

;;; Optional modules

;; Treesitter mode (Emacs 29+)
(when (>= emacs-major-version 29)
  (require 'emacs-flash-treesitter nil t))

(provide 'emacs-flash)
;;; emacs-flash.el ends here
