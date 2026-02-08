;;; emacs-flash-treesitter.el --- Treesitter integration for emacs-flash -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: chestnykh
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Treesitter-based navigation for emacs-flash.
;; Jump to and select treesitter nodes (functions, classes, blocks, etc.)
;;
;; Requires Emacs 29+ with treesitter support.
;;
;; Usage:
;;   M-x emacs-flash-treesitter     ; Show labels for nodes at point
;;   M-x emacs-flash-treesitter-search  ; Search + show treesitter nodes

;;; Code:

(require 'cl-lib)
(require 'emacs-flash-state)
(require 'emacs-flash-highlight)

;; Forward declarations
(declare-function treesit-node-at "treesit")
(declare-function treesit-node-parent "treesit")
(declare-function treesit-node-start "treesit")
(declare-function treesit-node-end "treesit")
(declare-function treesit-node-type "treesit")
(declare-function treesit-parser-list "treesit")
(declare-function treesit-buffer-root-node "treesit")
(declare-function treesit-available-p "treesit")

(defvar emacs-flash-labels)
(defvar emacs-flash-backdrop)

;;; Customization

(defgroup emacs-flash-treesitter nil
  "Treesitter navigation for emacs-flash."
  :group 'emacs-flash
  :prefix "emacs-flash-treesitter-")

(defcustom emacs-flash-treesitter-max-depth 10
  "Maximum depth of parent nodes to show."
  :type 'integer
  :group 'emacs-flash-treesitter)

;;; Node collection

(defun emacs-flash-treesitter--available-p ()
  "Return t if treesitter is available in this Emacs."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)))

(defun emacs-flash-treesitter--buffer-has-parser-p ()
  "Return t if current buffer has a treesitter parser."
  (and (emacs-flash-treesitter--available-p)
       (treesit-parser-list)))

(defun emacs-flash-treesitter--get-nodes-at-point ()
  "Get treesitter node at point and all its parents.
Returns list of (START END TYPE DEPTH) for each node."
  (when (emacs-flash-treesitter--buffer-has-parser-p)
    (let ((node (treesit-node-at (point)))
          (nodes '())
          (depth 0))
      (while (and node (< depth emacs-flash-treesitter-max-depth))
        (let ((start (treesit-node-start node))
              (end (treesit-node-end node))
              (type (treesit-node-type node)))
          ;; Skip anonymous nodes (punctuation, etc.)
          (when (and type (not (string-prefix-p "_" type)))
            (push (list start end type depth) nodes)))
        (setq node (treesit-node-parent node))
        (setq depth (1+ depth)))
      ;; Return from innermost to outermost (reverse to match flash.nvim)
      (nreverse nodes))))

(defun emacs-flash-treesitter--nodes-to-matches (nodes)
  "Convert NODES to emacs-flash-match structures with labels."
  (let ((labels emacs-flash-labels)
        (matches '())
        (idx 0))
    (dolist (node nodes)
      (let ((start (nth 0 node))
            (end (nth 1 node))
            (label (when (< idx (length labels))
                     (char-to-string (aref labels idx)))))
        (push (make-emacs-flash-match
               :pos (copy-marker start)
               :end-pos (copy-marker end)
               :label label
               :window (selected-window)
               :fold nil)
              matches)
        (setq idx (1+ idx))))
    (nreverse matches)))

;;; Highlighting

(defun emacs-flash-treesitter--highlight (state)
  "Highlight treesitter nodes in STATE."
  ;; Clear existing overlays
  (emacs-flash-highlight-clear state)
  ;; Create backdrop if enabled
  (when emacs-flash-backdrop
    (let ((ov (make-overlay (window-start) (window-end))))
      (overlay-put ov 'face 'emacs-flash-backdrop)
      (overlay-put ov 'emacs-flash t)
      (push ov (emacs-flash-state-overlays state))))
  ;; Show labels only (no underline/highlight for nodes - too noisy)
  (dolist (match (emacs-flash-state-matches state))
    (let* ((start (marker-position (emacs-flash-match-pos match)))
           (label (emacs-flash-match-label match)))
      (when label
        (let ((ov (make-overlay start (1+ start))))
          (overlay-put ov 'display (propertize label 'face 'emacs-flash-label))
          (overlay-put ov 'emacs-flash t)
          (overlay-put ov 'priority 200)
          (push ov (emacs-flash-state-overlays state)))))))

;;; Main commands

;;;###autoload
(defun emacs-flash-treesitter ()
  "Show labels for treesitter nodes at point.
Press a label to select that node's range."
  (interactive)
  (unless (emacs-flash-treesitter--available-p)
    (user-error "Treesitter is not available in this Emacs"))
  (unless (emacs-flash-treesitter--buffer-has-parser-p)
    (user-error "No treesitter parser for this buffer"))
  (let* ((nodes (emacs-flash-treesitter--get-nodes-at-point))
         (matches (emacs-flash-treesitter--nodes-to-matches nodes))
         (state (emacs-flash-state-create (list (selected-window)))))
    (unless nodes
      (user-error "No treesitter nodes at point"))
    (setf (emacs-flash-state-matches state) matches)
    (setf (emacs-flash-state-start-window state) (selected-window))
    (setf (emacs-flash-state-start-point state) (point))
    (unwind-protect
        (emacs-flash-treesitter--loop state)
      (emacs-flash-state-cleanup state))))

(defun emacs-flash-treesitter--loop (state)
  "Input loop for treesitter STATE."
  (catch 'emacs-flash-ts-done
    (while t
      (emacs-flash-treesitter--highlight state)
      (redisplay t)
      (let ((char (read-char "Treesitter node: ")))
        (cond
         ;; Escape - cancel
         ((= char ?\e)
          (throw 'emacs-flash-ts-done nil))
         ;; Enter - select innermost (first) node
         ((= char ?\r)
          (when-let ((match (car (emacs-flash-state-matches state))))
            (emacs-flash-treesitter--select-match match))
          (throw 'emacs-flash-ts-done t))
         ;; Try to find matching label
         (t
          (let* ((char-str (char-to-string char))
                 (match (cl-find-if (lambda (m)
                                      (equal (emacs-flash-match-label m) char-str))
                                    (emacs-flash-state-matches state))))
            (if match
                (progn
                  (emacs-flash-treesitter--select-match match)
                  (throw 'emacs-flash-ts-done t))
              (beep)))))))))

(defun emacs-flash-treesitter--select-match (match)
  "Select the range of MATCH."
  (let ((start (marker-position (emacs-flash-match-pos match)))
        (end (marker-position (emacs-flash-match-end-pos match))))
    ;; Set mark and move point to create selection
    (goto-char start)
    (push-mark end t t)
    (message "Selected %d characters" (- end start))))

;;; Evil integration

(declare-function evil-visual-make-region "evil-visual")
(declare-function evil-visual-char "evil-commands")
(defvar evil-visual-char)

;;;###autoload
(defun emacs-flash-treesitter-evil ()
  "Treesitter node selection with evil visual mode."
  (interactive)
  (unless (emacs-flash-treesitter--available-p)
    (user-error "Treesitter is not available in this Emacs"))
  (unless (emacs-flash-treesitter--buffer-has-parser-p)
    (user-error "No treesitter parser for this buffer"))
  (let* ((nodes (emacs-flash-treesitter--get-nodes-at-point))
         (matches (emacs-flash-treesitter--nodes-to-matches nodes))
         (state (emacs-flash-state-create (list (selected-window)))))
    (unless nodes
      (user-error "No treesitter nodes at point"))
    (setf (emacs-flash-state-matches state) matches)
    (setf (emacs-flash-state-start-window state) (selected-window))
    (setf (emacs-flash-state-start-point state) (point))
    (unwind-protect
        (emacs-flash-treesitter--evil-loop state)
      (emacs-flash-state-cleanup state))))

(defun emacs-flash-treesitter--evil-loop (state)
  "Input loop for treesitter STATE with evil support."
  (catch 'emacs-flash-ts-done
    (while t
      (emacs-flash-treesitter--highlight state)
      (redisplay t)
      (let ((char (read-char "Treesitter node: ")))
        (cond
         ;; Escape - cancel
         ((= char ?\e)
          (throw 'emacs-flash-ts-done nil))
         ;; Enter - select innermost node
         ((= char ?\r)
          (when-let ((match (car (emacs-flash-state-matches state))))
            (emacs-flash-treesitter--evil-select match))
          (throw 'emacs-flash-ts-done t))
         ;; Try to find matching label
         (t
          (let* ((char-str (char-to-string char))
                 (match (cl-find-if (lambda (m)
                                      (equal (emacs-flash-match-label m) char-str))
                                    (emacs-flash-state-matches state))))
            (if match
                (progn
                  (emacs-flash-treesitter--evil-select match)
                  (throw 'emacs-flash-ts-done t))
              (beep)))))))))

(defun emacs-flash-treesitter--evil-select (match)
  "Select the range of MATCH using evil visual mode."
  (let ((start (marker-position (emacs-flash-match-pos match)))
        (end (marker-position (emacs-flash-match-end-pos match))))
    (if (featurep 'evil)
        (progn
          (goto-char start)
          (evil-visual-char)
          (goto-char (1- end)))
      ;; Fallback for non-evil
      (goto-char start)
      (push-mark end t t))))

(provide 'emacs-flash-treesitter)
;;; emacs-flash-treesitter.el ends here
