;;; flash-treesitter.el --- Treesitter integration for flash -*- lexical-binding: t -*-

;; Copyright (C) 2025 Vadim Pavlov
;; Author: Vadim Pavlov <https://github.com/Prgebish>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Treesitter-based navigation for flash.
;; Jump to and select treesitter nodes (functions, classes, blocks, etc.)
;;
;; Requires Emacs 29+ with treesitter support.
;;
;; Usage:
;;   M-x flash-treesitter     ; Show labels for nodes at point

;;; Code:

(require 'cl-lib)
(require 'flash-state)
(require 'flash-highlight)

;; Forward declarations
(declare-function treesit-node-at "treesit")
(declare-function treesit-node-parent "treesit")
(declare-function treesit-node-start "treesit")
(declare-function treesit-node-end "treesit")
(declare-function treesit-node-type "treesit")
(declare-function treesit-parser-list "treesit")
(declare-function treesit-buffer-root-node "treesit")
(declare-function treesit-available-p "treesit")
(declare-function evil-visual-char "evil-commands")
(declare-function flash-match-pos-value "flash-state" (match))
(declare-function flash-match-end-pos-value "flash-state" (match))

(defvar flash-labels)
(defvar flash-backdrop)

;;; Customization

(defgroup flash-treesitter nil
  "Treesitter navigation for flash."
  :group 'flash
  :prefix "flash-treesitter-")

(defcustom flash-treesitter-max-depth 10
  "Maximum depth of parent nodes to show."
  :type 'integer
  :group 'flash-treesitter)

;;; Node collection

;; Treesit functions are called via `intern' so that:
;; 1. The package can declare (emacs "27.1") while treesitter needs 29.1
;; 2. package-lint doesn't flag optional treesit usage
(defun flash-treesitter--call (name &rest args)
  "Call treesitter function NAME with ARGS.
NAME is a string like \"treesit-node-at\"."
  (apply (intern name) args))

(defun flash-treesitter--available-p ()
  "Return t if treesitter is available in this Emacs."
  (and (fboundp (intern "treesit-available-p"))
       (flash-treesitter--call "treesit-available-p")))

(defun flash-treesitter--buffer-has-parser-p ()
  "Return t if current buffer has a treesitter parser."
  (and (flash-treesitter--available-p)
       (flash-treesitter--call "treesit-parser-list")))

(defun flash-treesitter--get-nodes-at-point ()
  "Get treesitter node at point and all its parents.
Returns list of (START END TYPE DEPTH) for each node."
  (when (flash-treesitter--buffer-has-parser-p)
    (let ((node (flash-treesitter--call "treesit-node-at" (point)))
          (nodes '())
          (depth 0))
      (while (and node (< depth flash-treesitter-max-depth))
        (let ((start (flash-treesitter--call "treesit-node-start" node))
              (end (flash-treesitter--call "treesit-node-end" node))
              (type (flash-treesitter--call "treesit-node-type" node)))
          ;; Skip anonymous nodes (punctuation, etc.)
          (when (and type (not (string-prefix-p "_" type)))
            (push (list start end type depth) nodes)))
        (setq node (flash-treesitter--call "treesit-node-parent" node))
        (setq depth (1+ depth)))
      ;; Return from innermost to outermost (reverse to match flash.nvim)
      (nreverse nodes))))

(defun flash-treesitter--nodes-to-matches (nodes)
  "Convert NODES to flash-match structures with labels."
  (let ((labels flash-labels)
        (matches '())
        (idx 0))
    (dolist (node nodes)
      (let ((start (nth 0 node))
            (end (nth 1 node))
            (label (when (< idx (length labels))
                     (char-to-string (aref labels idx)))))
        (push (make-flash-match
               :pos (copy-marker start)
               :end-pos (copy-marker end)
               :label label
               :window (selected-window)
               :fold nil)
              matches)
        (setq idx (1+ idx))))
    (nreverse matches)))

;;; Highlighting

(defun flash-treesitter--highlight (state)
  "Highlight treesitter nodes in STATE."
  ;; Clear existing overlays
  (flash-highlight-clear state)
  ;; Create backdrop if enabled
  (when flash-backdrop
    (let ((ov (make-overlay (window-start) (window-end))))
      (overlay-put ov 'face 'flash-backdrop)
      (overlay-put ov 'flash t)
      (push ov (flash-state-overlays state))))
  ;; Show labels only (no underline/highlight for nodes - too noisy)
  (dolist (match (flash-state-matches state))
    (let* ((start (flash-match-pos-value match))
           (label (flash-match-label match)))
      (when (and label (integerp start))
        (let ((ov (make-overlay start (1+ start))))
          (overlay-put ov 'display (propertize label 'face 'flash-label))
          (overlay-put ov 'flash t)
          (overlay-put ov 'priority 200)
          (push ov (flash-state-overlays state)))))))

;;; Main command

;;;###autoload
(defun flash-treesitter ()
  "Show labels for treesitter nodes at point.
Press a label to select that node's range.
With evil-mode, enters visual state for the selection."
  (interactive)
  (unless (flash-treesitter--available-p)
    (user-error "Treesitter is not available in this Emacs"))
  (unless (flash-treesitter--buffer-has-parser-p)
    (user-error "No treesitter parser for this buffer"))
  (let* ((nodes (flash-treesitter--get-nodes-at-point))
         (matches (flash-treesitter--nodes-to-matches nodes))
         (state (flash-state-create (list (selected-window)))))
    (unless nodes
      (user-error "No treesitter nodes at point"))
    (setf (flash-state-matches state) matches)
    (setf (flash-state-start-window state) (selected-window))
    (setf (flash-state-start-point state) (point))
    (unwind-protect
        (flash-treesitter--loop state)
      (flash-state-cleanup state))))

;;;###autoload
(defalias 'flash-treesitter-evil #'flash-treesitter
  "Deprecated alias for `flash-treesitter'.
`flash-treesitter' now auto-detects evil-mode.")

(defun flash-treesitter--loop (state)
  "Input loop for treesitter STATE."
  (catch 'flash-ts-done
    (while t
      (flash-treesitter--highlight state)
      (redisplay t)
      (let ((char (read-char "Treesitter node: ")))
        (cond
         ;; Escape - cancel
         ((= char ?\e)
          (throw 'flash-ts-done nil))
         ;; Enter - select innermost (first) node
         ((= char ?\r)
          (when-let ((match (car (flash-state-matches state))))
            (flash-treesitter--select-match match))
          (throw 'flash-ts-done t))
         ;; Try to find matching label
         (t
          (let* ((char-str (char-to-string char))
                 (match (cl-find-if (lambda (m)
                                      (equal (flash-match-label m) char-str))
                                    (flash-state-matches state))))
            (if match
                (progn
                  (flash-treesitter--select-match match)
                  (throw 'flash-ts-done t))
              (beep)))))))))

(defun flash-treesitter--select-match (match)
  "Select the range of MATCH.
Uses evil visual mode when evil is loaded, otherwise sets Emacs region."
  (let ((start (flash-match-pos-value match))
        (end (flash-match-end-pos-value match)))
    (unless (and (integerp start) (integerp end))
      (user-error "Invalid treesitter match range"))
    (if (featurep 'evil)
        (progn
          (goto-char start)
          (evil-visual-char)
          (goto-char (1- end)))
      (goto-char start)
      (push-mark end t t)
      (message "Selected %d characters" (- end start)))))

(provide 'flash-treesitter)
;;; flash-treesitter.el ends here
