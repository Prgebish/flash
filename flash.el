;;; flash.el --- Flash-style navigation for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: chestnykh
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, navigation
;; URL: https://github.com/chestnykh/flash

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
;;   M-x flash-jump
;;
;; Or bind to a key:
;;   (global-set-key (kbd "s-j") #'flash-jump)

;;; Code:

(require 'cl-lib)
(require 'flash-state)
(require 'flash-search)
(require 'flash-label)
(require 'flash-highlight)
(require 'flash-jump)

;;; Customization

(defgroup flash nil
  "Flash-style navigation."
  :group 'convenience
  :prefix "flash-")

(defcustom flash-labels "asdfjkl;ghqwertyuiopzxcvbnm"
  "Characters to use as jump labels, ordered by priority.
Earlier characters are assigned to closer matches."
  :type 'string
  :group 'flash)

(defcustom flash-label-uppercase nil
  "When non-nil, also use uppercase versions of labels.
This doubles the number of available single-character labels.
Lowercase labels are assigned first, then uppercase.
Uppercase labels never conflict with search continuation."
  :type 'boolean
  :group 'flash)

(defcustom flash-multi-char-labels nil
  "When non-nil, use multi-character labels (aa, as, ad...) when needed.
Multi-char labels are used when matches exceed available single-char labels.
When nil, only single-character labels are used and excess matches stay
unlabeled."
  :type 'boolean
  :group 'flash)

(defcustom flash-multi-window t
  "When non-nil, search in all visible windows."
  :type 'boolean
  :group 'flash)

(defcustom flash-autojump nil
  "When non-nil, automatically jump when there is only one match.
Disabled by default to prevent accidental text modification."
  :type 'boolean
  :group 'flash)

(defcustom flash-backdrop t
  "When non-nil, dim non-matching text with backdrop effect."
  :type 'boolean
  :group 'flash)

(defcustom flash-case-fold t
  "When non-nil, ignore case when searching."
  :type 'boolean
  :group 'flash)

(defcustom flash-rainbow nil
  "When non-nil, use different colors for each label.
Makes labels more visually distinct."
  :type 'boolean
  :group 'flash)

(defcustom flash-rainbow-shade 2
  "Shade level for rainbow labels (1-9).
Controls label brightness using Tailwind CSS color shades:
  1-2: pastel backgrounds, dark text
  3-4: medium backgrounds, dark text
  5: saturated backgrounds, very dark text (flash.nvim default)
  6-9: dark backgrounds, light text
Only effective when `flash-rainbow' is non-nil."
  :type 'integer
  :group 'flash)

(defcustom flash-highlight-matches t
  "When non-nil, highlight matched text.
When nil, only labels are shown, keeping original syntax highlighting."
  :type 'boolean
  :group 'flash)

(defcustom flash-label-position 'overlay
  "Where to display the jump label relative to the match.
- `overlay': Label replaces the first character of the match (default)
- `after': Label appears after the match (flash.nvim default)
- `before': Label appears before the match
- `eol': Label appears at end of line"
  :type '(choice (const :tag "Over first char" overlay)
                 (const :tag "After match" after)
                 (const :tag "Before match" before)
                 (const :tag "End of line" eol))
  :group 'flash)

(defcustom flash-jump-position 'start
  "Where to place cursor after jumping to a match.
- `start': Cursor at the beginning of the match (default)
- `end': Cursor at the end of the match"
  :type '(choice (const :tag "Start of match" start)
                 (const :tag "End of match" end))
  :group 'flash)

(defcustom flash-jumplist t
  "When non-nil, save position to mark ring before jumping.
This allows returning to the previous position with `C-u C-SPC'
or `C-x C-SPC' (global mark), or with evil's `C-o'."
  :type 'boolean
  :group 'flash)

(defcustom flash-search-history nil
  "When non-nil, add search pattern to Emacs search history.
The pattern will appear in `isearch' history."
  :type 'boolean
  :group 'flash)

(defcustom flash-nohlsearch nil
  "When non-nil, clear search highlighting after jump.
Works with both isearch and evil-search highlighting."
  :type 'boolean
  :group 'flash)

(defcustom flash-min-pattern-length 0
  "Minimum pattern length to show jump labels.
Labels won't appear until the pattern reaches this length.
Set to 0 to always show labels (default)."
  :type 'integer
  :group 'flash)

;;; State for continue functionality

(defvar flash--last-pattern nil
  "Last search pattern used in flash jump.")

;;; Forward declarations for evil
(defvar evil-ex-search-history)
(defvar evil-search-module)
(defvar evil-search-forward-history)

(defun flash--add-to-evil-search-history (pattern)
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
(defun flash-jump ()
  "Start flash jump session.
Type characters to search, then press a label to jump.
Press RET to jump to first match, ESC to cancel."
  (interactive)
  (let* ((windows (if flash-multi-window
                      (window-list nil 'no-minibuf)
                    (list (selected-window))))
         (state (flash-state-create windows)))
    (setf (flash-state-start-window state) (selected-window))
    (setf (flash-state-start-point state) (point))
    (unwind-protect
        (flash--loop state)
      (flash-state-cleanup state))))

;;; Main Loop

(defun flash--loop (state)
  "Main input loop for STATE.
Returns t if jump was made, nil if cancelled."
  (catch 'flash-done
    (while t
      ;; Update search results
      (flash-search state)

      ;; Assign labels (respecting min-pattern-length)
      (if (>= (length (flash-state-pattern state))
              flash-min-pattern-length)
          (flash-label-matches state)
        ;; Clear labels if pattern too short
        (dolist (match (flash-state-matches state))
          (setf (flash-match-label match) nil)))

      ;; Update display
      (flash-highlight-update state)

      ;; Autojump if single match (and pattern long enough)
      (when (and flash-autojump
                 (= (length (flash-state-matches state)) 1)
                 (>= (length (flash-state-pattern state))
                     flash-min-pattern-length))
        (flash--do-jump state)
        (throw 'flash-done t))

      ;; Read input
      (redisplay t)  ; Force display update before reading
      (let* ((pattern (flash-state-pattern state))
             (prefix (flash-state-label-prefix state))
             (match-count (length (flash-state-matches state)))
             (prompt (flash--format-prompt pattern match-count prefix))
             (char (read-char prompt))
             (char-str (char-to-string char)))
        (cond
         ;; Escape - cancel
         ((= char ?\e)
          (flash-return-to-start state)
          (throw 'flash-done nil))

         ;; Enter - jump to first match
         ((= char ?\r)
          (when (flash-state-matches state)
            (flash--do-jump state))
          (throw 'flash-done t))

         ;; Backspace - delete last char or clear prefix
         ((or (= char ?\C-?) (= char ?\C-h) (= char 127))
          (cond
           (prefix
            ;; Clear prefix first
            (setf (flash-state-label-prefix state) nil))
           ((> (length pattern) 0)
            ;; Then delete from pattern
            (setf (flash-state-pattern state)
                  (substring pattern 0 -1)))))

         ;; Check if it completes a label (with current prefix)
         ((let ((full-label (concat (or prefix "") char-str)))
            (flash-jump-to-label state full-label))
          (flash--save-pattern state)
          (throw 'flash-done t))

         ;; Check if it's a valid label prefix (for multi-char labels)
         ((flash--valid-label-prefix-p state char-str)
          (setf (flash-state-label-prefix state) char-str))

         ;; Add to pattern (only if no prefix active)
         ((not prefix)
          (setf (flash-state-pattern state)
                (concat pattern char-str)))

         ;; Invalid input with prefix - ignore or beep
         (t (beep)))))))

(defun flash--do-jump (state)
  "Perform jump to first match in STATE.
Also saves pattern and adds to search history."
  (flash-jump-to-first state)
  (flash--save-pattern state))

(defun flash--save-pattern (state)
  "Save pattern from STATE for continue and search history."
  (let ((pattern (flash-state-pattern state)))
    (when (> (length pattern) 0)
      ;; Save for continue
      (setq flash--last-pattern pattern)
      ;; Add to search history
      (when flash-search-history
        ;; Emacs isearch history
        (isearch-update-ring pattern)
        ;; Evil search history (if available)
        (when (featurep 'evil)
          (flash--add-to-evil-search-history pattern))))))

(defun flash--format-prompt (pattern match-count &optional prefix)
  "Format prompt string showing PATTERN, MATCH-COUNT, and optional PREFIX."
  (let ((base (if (string-empty-p pattern)
                  "Flash: "
                (format "Flash [%s] (%d): " pattern match-count))))
    (if prefix
        (concat base prefix)
      base)))

(defun flash--valid-label-prefix-p (state prefix)
  "Return t if PREFIX is the start of any multi-char label in STATE."
  (cl-some (lambda (match)
             (let ((label (flash-match-label match)))
               (and label
                    (> (length label) 1)
                    (string-prefix-p prefix label))))
           (flash-state-matches state)))

;;;###autoload
(defun flash-jump-continue ()
  "Continue flash jump with the last search pattern.
If no previous pattern exists, starts a new search."
  (interactive)
  (let* ((windows (if flash-multi-window
                      (window-list nil 'no-minibuf)
                    (list (selected-window))))
         (state (flash-state-create windows)))
    (setf (flash-state-start-window state) (selected-window))
    (setf (flash-state-start-point state) (point))
    ;; Set initial pattern from last search
    (when flash--last-pattern
      (setf (flash-state-pattern state) flash--last-pattern))
    (unwind-protect
        (flash--loop state)
      (flash-state-cleanup state))))

;;; Optional modules

;; Treesitter mode (Emacs 29+)
(when (>= emacs-major-version 29)
  (require 'flash-treesitter nil t))

(provide 'flash)
;;; flash.el ends here
