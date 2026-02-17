;;; flash-isearch.el --- Search integration for flash -*- lexical-binding: t -*-

;; Copyright (C) 2025 Vadim Pavlov
;; Author: Vadim Pavlov <https://github.com/Prgebish>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Flash labels during incremental search (evil-ex-search and isearch).
;; Like flash.nvim search mode.
;;
;; Usage:
;;   (require 'flash-isearch)
;;   (flash-isearch-mode 1)
;;
;; During search:
;;   - Labels appear on all matches
;;   - Press a label character to jump directly to that match
;;   - C-; to toggle flash labels on/off

;;; Code:

(require 'cl-lib)
(require 'flash)

;;; Customization

(defgroup flash-isearch nil
  "Flash labels during search."
  :group 'flash
  :prefix "flash-isearch-")

(defcustom flash-isearch-enabled t
  "When non-nil, show flash labels during search by default."
  :type 'boolean
  :group 'flash-isearch)

(defcustom flash-isearch-toggle-key "C-;"
  "Key to toggle flash labels during search.
Set to nil to disable toggle functionality."
  :type '(choice (string :tag "Key")
                 (const :tag "Disabled" nil))
  :group 'flash-isearch)

(defcustom flash-isearch-trigger nil
  "Character that activates label jumping during search.
When nil, labels work directly - smart skip prevents conflicts
by not assigning labels that would continue the search pattern.
When set (e.g. \";\"), you must type trigger + label to jump."
  :type '(choice (const :tag "No trigger - use smart skip" nil)
                 (string :tag "Trigger character"))
  :group 'flash-isearch)

;;; State Variables

(defvar flash-isearch--state nil
  "Current flash state during search.")

(defvar flash-isearch--active nil
  "Non-nil when flash search is actively showing labels.")

(defvar flash-isearch--in-session nil
  "Non-nil when inside a search session (for keymap activation).")

(defvar flash-isearch--label-mode nil
  "Non-nil when trigger was pressed and next char is label.")

(defvar flash-isearch--original-buffer nil
  "Buffer where search started.")

;;; Forward declarations
(defvar evil-ex-search-direction)
(defvar evil-ex-search-start-point)
(defvar evil-ex-original-buffer)
(defvar isearch-string)
(defvar isearch-forward)
(defvar isearch-mode-map)

(declare-function isearch-exit "isearch")

;;; Core Functions

(defun flash-isearch--start ()
  "Start flash search mode."
  (when flash-isearch-enabled
    (setq flash-isearch--active t)
    (setq flash-isearch--in-session t)
    (setq flash-isearch--original-buffer (current-buffer))
    (let ((state (flash-state-create
                  (if (bound-and-true-p flash-multi-window)
                      (window-list nil 'no-minibuf)
                    (list (selected-window))))))
      ;; Search integration: check label conflicts in whole buffer
      ;; because search can jump to matches anywhere, not just visible area
      (setf (flash-state-whole-buffer state) t)
      (setq flash-isearch--state state))))

(defun flash-isearch--stop ()
  "Stop flash search mode and clean up."
  (when flash-isearch--state
    (flash-highlight-clear flash-isearch--state)
    (flash-state-cleanup flash-isearch--state))
  (setq flash-isearch--state nil)
  (setq flash-isearch--active nil)
  (setq flash-isearch--in-session nil)
  (setq flash-isearch--label-mode nil)
  (setq flash-isearch--original-buffer nil))

(defun flash-isearch--update (pattern)
  "Update flash labels for PATTERN."
  (when (and flash-isearch--active
             flash-isearch--state)
    (with-current-buffer (or flash-isearch--original-buffer
                             (current-buffer))
      ;; Clear old overlays first
      (flash-highlight-clear flash-isearch--state)
      ;; Update pattern, then refresh or clear matches.
      (setf (flash-state-pattern flash-isearch--state) pattern)
      (if (> (length pattern) 0)
          (progn
            (flash-search flash-isearch--state)
            ;; Assign labels.
            (flash-label-matches flash-isearch--state)
            ;; Update display.
            (flash-highlight-update flash-isearch--state))
        ;; Empty pattern should clear labels and stale overlays.
        (setf (flash-state-matches flash-isearch--state) nil)))))

(defun flash-isearch--toggle ()
  "Toggle flash labels during search."
  (interactive)
  (if flash-isearch--active
      (progn
        (when flash-isearch--state
          (flash-highlight-clear flash-isearch--state))
        (setq flash-isearch--active nil)
        (message "Flash labels: OFF"))
    (setq flash-isearch--active t)
    (message "Flash labels: ON")
    ;; Re-update with current pattern
    (cond
     ;; Evil search
     ((and (bound-and-true-p evil-ex-original-buffer)
           (minibufferp))
      (flash-isearch--update (minibuffer-contents-no-properties)))
     ;; Isearch
     ((bound-and-true-p isearch-mode)
      (flash-isearch--update isearch-string)))))

(defvar flash-isearch--pending-match nil
  "Serialized match snapshot to jump to after exiting search.")

(defun flash-isearch--snapshot-match (match)
  "Return stable jump snapshot for MATCH."
  (let* ((pos-marker (flash-match-pos match))
         (end-marker (flash-match-end-pos match))
         (pos (and (markerp pos-marker) (marker-position pos-marker)))
         (end-pos (and (markerp end-marker) (marker-position end-marker)))
         (buffer (and (markerp pos-marker) (marker-buffer pos-marker)))
         (window (flash-match-window match)))
    (when (and (integerp pos)
               (integerp end-pos)
               (buffer-live-p buffer))
      (list :buffer buffer
            :window (and (window-live-p window) window)
            :pos pos
            :end-pos end-pos
            :fold (flash-match-fold match)))))

(defun flash-isearch--snapshot-to-match (snapshot)
  "Convert SNAPSHOT back into an `flash-match'."
  (let ((buffer (plist-get snapshot :buffer))
        (window (plist-get snapshot :window))
        (pos (plist-get snapshot :pos))
        (end-pos (plist-get snapshot :end-pos))
        (fold (plist-get snapshot :fold)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (and (integerp pos)
                   (integerp end-pos)
                   (<= (point-min) pos)
                   (<= pos (point-max))
                   (<= (point-min) end-pos)
                   (<= end-pos (1+ (point-max))))
          (make-flash-match
           :pos (copy-marker pos)
           :end-pos (copy-marker end-pos)
           :label nil
           :window (if (window-live-p window) window (selected-window))
           :fold fold))))))

(defun flash-isearch--trigger-char ()
  "Return label trigger character, or nil when disabled/invalid."
  (when (and (stringp flash-isearch-trigger)
             (= (length flash-isearch-trigger) 1))
    (string-to-char flash-isearch-trigger)))

(defun flash-isearch--try-jump (char)
  "Try to jump to label CHAR.  Return t if jumped, nil otherwise."
  (when (and flash-isearch--active
             flash-isearch--state)
    (let ((match (flash-find-match-by-label
                  flash-isearch--state (char-to-string char))))
      (when-let ((snapshot (and match
                                (flash-isearch--snapshot-match match))))
        ;; Save match for jumping after minibuffer exits
        (setq flash-isearch--pending-match snapshot)
        ;; Clean up flash overlays
        (flash-isearch--stop)
        t))))

(defun flash-isearch--do-pending-jump ()
  "Perform pending jump after search exit."
  (when flash-isearch--pending-match
    (let ((snapshot flash-isearch--pending-match))
      (setq flash-isearch--pending-match nil)
      (when-let ((match (flash-isearch--snapshot-to-match snapshot)))
        (unwind-protect
            (flash-jump-to-match match)
          (when (markerp (flash-match-pos match))
            (set-marker (flash-match-pos match) nil))
          (when (markerp (flash-match-end-pos match))
            (set-marker (flash-match-end-pos match) nil)))))))

;;; Evil Integration

(defvar flash-isearch--evil-minibuffer-hook nil
  "Hook function for evil minibuffer setup.")

(defun flash-isearch--evil-start-advice (&rest _)
  "Start flash at evil search entry."
  (setq flash-isearch--original-buffer
        (or (bound-and-true-p evil-ex-original-buffer)
            (current-buffer)))
  (flash-isearch--start))

(defun flash-isearch--evil-stop-advice (&rest _)
  "Advice to stop flash when evil search ends."
  (flash-isearch--stop)
  ;; Perform pending jump after search cleanup
  (run-at-time 0 nil #'flash-isearch--do-pending-jump))

(defun flash-isearch--evil-update ()
  "Update flash during evil search (called from `after-change-functions')."
  (when (and flash-isearch--active
             (minibufferp)
             (bound-and-true-p evil-ex-original-buffer))
    (let ((pattern (minibuffer-contents-no-properties)))
      (when (> (length pattern) 0)
        (with-current-buffer evil-ex-original-buffer
          (flash-isearch--update pattern))))))

(defun flash-isearch--evil-pre-command ()
  "Check if user pressed trigger or label key.
With trigger: press trigger char to activate labels, then press label to jump.
Without trigger: any label char jumps (only when multiple matches)."
  (when (and flash-isearch--active
             flash-isearch--state
             (characterp last-command-event))
    (let ((trigger (flash-isearch--trigger-char)))
      (cond
       ;; Label mode is active - next char is the label
       (flash-isearch--label-mode
        (setq flash-isearch--label-mode nil)
        (when (flash-isearch--try-jump last-command-event)
          (exit-minibuffer)))
       ;; Trigger character pressed - activate label mode
       ((and trigger
             (= last-command-event trigger)
             (> (length (flash-state-matches flash-isearch--state)) 0))
        (setq flash-isearch--label-mode t)
        ;; Show indicator in minibuffer
        (minibuffer-message " [label?]")
        ;; Consume the trigger - don't add to search pattern
        (setq this-command 'ignore))
       ;; No trigger configured - try jump if label matches
       ((and (null trigger)
             (flash-state-matches flash-isearch--state))
        (when (flash-isearch--try-jump last-command-event)
          (exit-minibuffer)))))))

(defvar flash-isearch--emulation-alist nil
  "Alist for `emulation-mode-map-alists' to override evil keymaps.")

(defun flash-isearch--evil-after-change (&rest _)
  "Trigger flash update after minibuffer change during evil search."
  (run-with-idle-timer 0 nil #'flash-isearch--evil-update))

(defun flash-isearch--evil-minibuffer-setup ()
  "Setup hooks in minibuffer for evil search."
  (when (bound-and-true-p evil-ex-original-buffer)
    (add-hook 'after-change-functions
              #'flash-isearch--evil-after-change
              nil t)
    (add-hook 'pre-command-hook
              #'flash-isearch--evil-pre-command nil t)
    ;; Bind toggle key with highest priority using emulation-mode-map-alists
    ;; This overrides even evil state maps
    (when flash-isearch-toggle-key
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd flash-isearch-toggle-key)
                    #'flash-isearch--toggle)
        (setq flash-isearch--emulation-alist
              `((flash-isearch--in-session . ,map)))
        (unless (memq 'flash-isearch--emulation-alist
                      emulation-mode-map-alists)
          (push 'flash-isearch--emulation-alist
                emulation-mode-map-alists))))))

(defvar evil-ex-search-keymap)

(defun flash-isearch--setup-evil ()
  "Set up evil-ex-search integration."
  (advice-add 'evil-ex-search-start-session :after
              #'flash-isearch--evil-start-advice)
  (advice-add 'evil-ex-search-stop-session :before
              #'flash-isearch--evil-stop-advice)
  (add-hook 'minibuffer-setup-hook
            #'flash-isearch--evil-minibuffer-setup)
  ;; Toggle key for evil search
  (when (and flash-isearch-toggle-key
             (boundp 'evil-ex-search-keymap))
    (define-key evil-ex-search-keymap
                (kbd flash-isearch-toggle-key)
                #'flash-isearch--toggle)))

(defun flash-isearch--teardown-evil ()
  "Remove evil-ex-search integration."
  (advice-remove 'evil-ex-search-start-session
                 #'flash-isearch--evil-start-advice)
  (advice-remove 'evil-ex-search-stop-session
                 #'flash-isearch--evil-stop-advice)
  (remove-hook 'minibuffer-setup-hook
               #'flash-isearch--evil-minibuffer-setup)
  ;; Remove toggle key
  (when (and flash-isearch-toggle-key
             (boundp 'evil-ex-search-keymap))
    (define-key evil-ex-search-keymap
                (kbd flash-isearch-toggle-key) nil))
  (setq emulation-mode-map-alists
        (delq 'flash-isearch--emulation-alist
              emulation-mode-map-alists))
  (setq flash-isearch--emulation-alist nil))

;;; Isearch Integration

(defun flash-isearch--isearch-start ()
  "Start flash at isearch entry."
  (flash-isearch--start))

(defun flash-isearch--isearch-end ()
  "Stop flash when isearch ends."
  (flash-isearch--stop)
  ;; Perform pending jump after search cleanup
  (run-at-time 0 nil #'flash-isearch--do-pending-jump))

(defun flash-isearch--isearch-update ()
  "Update flash during isearch."
  (when (and flash-isearch--active
             (bound-and-true-p isearch-string))
    (flash-isearch--update isearch-string)))

(defun flash-isearch--printing-char-advice (orig-fun &rest args)
  "Advice for `isearch-printing-char' to intercept label keys.
ORIG-FUN is the original function, ARGS are passed through."
  (if (and flash-isearch--active
           flash-isearch--state
           (characterp last-command-event))
      (let ((trigger (flash-isearch--trigger-char)))
        (cond
         ;; Label mode is active - next char is the label
         (flash-isearch--label-mode
          (setq flash-isearch--label-mode nil)
          (if (flash-isearch--try-jump last-command-event)
              (isearch-exit)
            ;; Not a valid label, continue with normal input
            (apply orig-fun args)))
         ;; Trigger character pressed - activate label mode
         ((and trigger
               (= last-command-event trigger)
               (> (length (flash-state-matches flash-isearch--state)) 0))
          (setq flash-isearch--label-mode t)
          (message "[label?]"))
         ;; No trigger configured - try jump if label matches
         ((and (null trigger)
               (flash-state-matches flash-isearch--state))
          (if (flash-isearch--try-jump last-command-event)
              (isearch-exit)
            ;; Not a valid label, continue with normal input
            (apply orig-fun args)))
         ;; Default - normal isearch behavior
         (t (apply orig-fun args))))
    ;; Flash not active - normal behavior
    (apply orig-fun args)))

(defun flash-isearch--setup-isearch ()
  "Set up isearch integration."
  (add-hook 'isearch-mode-hook #'flash-isearch--isearch-start)
  (add-hook 'isearch-mode-end-hook #'flash-isearch--isearch-end)
  (add-hook 'isearch-update-post-hook #'flash-isearch--isearch-update)
  ;; Advice to intercept label keys
  (advice-add 'isearch-printing-char :around
              #'flash-isearch--printing-char-advice)
  ;; Toggle key
  (when flash-isearch-toggle-key
    (define-key isearch-mode-map (kbd flash-isearch-toggle-key)
                #'flash-isearch--toggle)))

(defun flash-isearch--teardown-isearch ()
  "Remove isearch integration."
  (remove-hook 'isearch-mode-hook #'flash-isearch--isearch-start)
  (remove-hook 'isearch-mode-end-hook #'flash-isearch--isearch-end)
  (remove-hook 'isearch-update-post-hook #'flash-isearch--isearch-update)
  ;; Remove advice
  (advice-remove 'isearch-printing-char
                 #'flash-isearch--printing-char-advice)
  (when (and flash-isearch-toggle-key
             (boundp 'isearch-mode-map))
    (define-key isearch-mode-map (kbd flash-isearch-toggle-key) nil)))

;;; Minor Mode

;;;###autoload
(define-minor-mode flash-isearch-mode
  "Show flash labels during incremental search.
Works with both evil-ex-search and isearch."
  :global t
  :lighter " FlashS"
  :group 'flash-isearch
  (if flash-isearch-mode
      (progn
        (when (featurep 'evil)
          (flash-isearch--setup-evil))
        (flash-isearch--setup-isearch))
    (flash-isearch--stop)
    (when (featurep 'evil)
      (flash-isearch--teardown-evil))
    (flash-isearch--teardown-isearch)))

(provide 'flash-isearch)
;;; flash-isearch.el ends here
