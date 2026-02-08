;;; emacs-flash-isearch.el --- Search integration for emacs-flash -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: chestnykh
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;; Flash labels during incremental search (evil-ex-search and isearch).
;; Like flash.nvim search mode.
;;
;; Usage:
;;   (require 'emacs-flash-isearch)
;;   (emacs-flash-isearch-mode 1)
;;
;; During search:
;;   - Labels appear on all matches
;;   - Press a label character to jump directly to that match
;;   - C-; to toggle flash labels on/off

;;; Code:

(require 'cl-lib)
(require 'emacs-flash-state)
(require 'emacs-flash-search)
(require 'emacs-flash-label)
(require 'emacs-flash-highlight)
(require 'emacs-flash-jump)

;;; Customization

(defgroup emacs-flash-isearch nil
  "Flash labels during search."
  :group 'emacs-flash
  :prefix "emacs-flash-isearch-")

(defcustom emacs-flash-isearch-enabled t
  "When non-nil, show flash labels during search by default."
  :type 'boolean
  :group 'emacs-flash-isearch)

(defcustom emacs-flash-isearch-toggle-key "C-;"
  "Key to toggle flash labels during search.
Set to nil to disable toggle functionality."
  :type '(choice (string :tag "Key")
                 (const :tag "Disabled" nil))
  :group 'emacs-flash-isearch)

(defcustom emacs-flash-isearch-trigger nil
  "Character that activates label jumping during search.
When nil, labels work directly - smart skip prevents conflicts
by not assigning labels that would continue the search pattern.
When set (e.g. \";\"), you must type trigger + label to jump."
  :type '(choice (const :tag "No trigger - use smart skip" nil)
                 (string :tag "Trigger character"))
  :group 'emacs-flash-isearch)

;;; State Variables

(defvar emacs-flash-isearch--state nil
  "Current flash state during search.")

(defvar emacs-flash-isearch--active nil
  "Non-nil when flash search is actively showing labels.")

(defvar emacs-flash-isearch--in-session nil
  "Non-nil when inside a search session (for keymap activation).")

(defvar emacs-flash-isearch--label-mode nil
  "Non-nil when trigger was pressed and next char is label.")

(defvar emacs-flash-isearch--original-buffer nil
  "Buffer where search started.")

;;; Forward declarations
(defvar evil-ex-search-direction)
(defvar evil-ex-search-start-point)
(defvar evil-ex-original-buffer)
(defvar isearch-string)
(defvar isearch-forward)
(defvar isearch-mode-map)
(defvar emacs-flash-multi-window)

(declare-function isearch-exit "isearch")

;;; Core Functions

(defun emacs-flash-isearch--start ()
  "Start flash search mode."
  (when emacs-flash-isearch-enabled
    (setq emacs-flash-isearch--active t)
    (setq emacs-flash-isearch--in-session t)
    (setq emacs-flash-isearch--original-buffer (current-buffer))
    (let ((state (emacs-flash-state-create
                  (if (bound-and-true-p emacs-flash-multi-window)
                      (window-list nil 'no-minibuf)
                    (list (selected-window))))))
      ;; Search integration: check label conflicts in whole buffer
      ;; because search can jump to matches anywhere, not just visible area
      (setf (emacs-flash-state-whole-buffer state) t)
      (setq emacs-flash-isearch--state state))))

(defun emacs-flash-isearch--stop ()
  "Stop flash search mode and clean up."
  (when emacs-flash-isearch--state
    (emacs-flash-highlight-clear emacs-flash-isearch--state)
    (emacs-flash-state-cleanup emacs-flash-isearch--state))
  (setq emacs-flash-isearch--state nil)
  (setq emacs-flash-isearch--active nil)
  (setq emacs-flash-isearch--in-session nil)
  (setq emacs-flash-isearch--label-mode nil)
  (setq emacs-flash-isearch--original-buffer nil))

(defun emacs-flash-isearch--update (pattern)
  "Update flash labels for PATTERN."
  (when (and emacs-flash-isearch--active
             emacs-flash-isearch--state
             (> (length pattern) 0))
    (with-current-buffer (or emacs-flash-isearch--original-buffer
                             (current-buffer))
      ;; Clear old overlays first
      (emacs-flash-highlight-clear emacs-flash-isearch--state)
      ;; Update pattern and search
      (setf (emacs-flash-state-pattern emacs-flash-isearch--state) pattern)
      (emacs-flash-search emacs-flash-isearch--state)
      ;; Assign labels
      (emacs-flash-label-matches emacs-flash-isearch--state)
      ;; Update display
      (emacs-flash-highlight-update emacs-flash-isearch--state))))

(defun emacs-flash-isearch--toggle ()
  "Toggle flash labels during search."
  (interactive)
  (if emacs-flash-isearch--active
      (progn
        (when emacs-flash-isearch--state
          (emacs-flash-highlight-clear emacs-flash-isearch--state))
        (setq emacs-flash-isearch--active nil)
        (message "Flash labels: OFF"))
    (setq emacs-flash-isearch--active t)
    (message "Flash labels: ON")
    ;; Re-update with current pattern
    (cond
     ;; Evil search
     ((and (bound-and-true-p evil-ex-original-buffer)
           (minibufferp))
      (emacs-flash-isearch--update (minibuffer-contents-no-properties)))
     ;; Isearch
     ((bound-and-true-p isearch-mode)
      (emacs-flash-isearch--update isearch-string)))))

(defvar emacs-flash-isearch--pending-match nil
  "Match to jump to after exiting search.")

(defun emacs-flash-isearch--try-jump (char)
  "Try to jump to label CHAR. Return t if jumped, nil otherwise."
  (when (and emacs-flash-isearch--active
             emacs-flash-isearch--state)
    (let ((match (emacs-flash-find-match-by-label
                  emacs-flash-isearch--state (char-to-string char))))
      (when match
        ;; Save match for jumping after minibuffer exits
        (setq emacs-flash-isearch--pending-match match)
        ;; Clean up flash overlays
        (emacs-flash-isearch--stop)
        t))))

(defun emacs-flash-isearch--do-pending-jump ()
  "Perform pending jump after search exit."
  (when emacs-flash-isearch--pending-match
    (let ((match emacs-flash-isearch--pending-match))
      (setq emacs-flash-isearch--pending-match nil)
      (emacs-flash-jump-to-match match))))

;;; Evil Integration

(defvar emacs-flash-isearch--evil-minibuffer-hook nil
  "Hook function for evil minibuffer setup.")

(defun emacs-flash-isearch--evil-start-advice (&rest _)
  "Advice to start flash when evil search starts."
  (setq emacs-flash-isearch--original-buffer
        (or (bound-and-true-p evil-ex-original-buffer)
            (current-buffer)))
  (emacs-flash-isearch--start))

(defun emacs-flash-isearch--evil-stop-advice (&rest _)
  "Advice to stop flash when evil search ends."
  (emacs-flash-isearch--stop)
  ;; Perform pending jump after search cleanup
  (run-at-time 0 nil #'emacs-flash-isearch--do-pending-jump))

(defun emacs-flash-isearch--evil-update ()
  "Update flash during evil search (called from after-change-functions)."
  (when (and emacs-flash-isearch--active
             (minibufferp)
             (bound-and-true-p evil-ex-original-buffer))
    (let ((pattern (minibuffer-contents-no-properties)))
      (when (> (length pattern) 0)
        (with-current-buffer evil-ex-original-buffer
          (emacs-flash-isearch--update pattern))))))

(defun emacs-flash-isearch--evil-pre-command ()
  "Check if user pressed trigger or label key.
With trigger: press trigger char to activate labels, then press label to jump.
Without trigger: any label char jumps (only when multiple matches)."
  (when (and emacs-flash-isearch--active
             emacs-flash-isearch--state
             (characterp last-command-event))
    (cond
     ;; Label mode is active - next char is the label
     (emacs-flash-isearch--label-mode
      (setq emacs-flash-isearch--label-mode nil)
      (when (emacs-flash-isearch--try-jump last-command-event)
        (exit-minibuffer)))
     ;; Trigger character pressed - activate label mode
     ((and emacs-flash-isearch-trigger
           (= last-command-event (string-to-char emacs-flash-isearch-trigger))
           (> (length (emacs-flash-state-matches emacs-flash-isearch--state)) 0))
      (setq emacs-flash-isearch--label-mode t)
      ;; Show indicator in minibuffer
      (minibuffer-message " [label?]")
      ;; Consume the trigger - don't add to search pattern
      (setq this-command 'ignore))
     ;; No trigger configured - try jump if label matches
     ((and (null emacs-flash-isearch-trigger)
           (emacs-flash-state-matches emacs-flash-isearch--state))
      (when (emacs-flash-isearch--try-jump last-command-event)
        (exit-minibuffer))))))

(defvar emacs-flash-isearch--emulation-alist nil
  "Alist for `emulation-mode-map-alists' to override evil keymaps.")

(defun emacs-flash-isearch--evil-after-change (&rest _)
  "Trigger flash update after minibuffer change during evil search."
  (run-with-idle-timer 0 nil #'emacs-flash-isearch--evil-update))

(defun emacs-flash-isearch--evil-minibuffer-setup ()
  "Setup hooks in minibuffer for evil search."
  (when (bound-and-true-p evil-ex-original-buffer)
    (add-hook 'after-change-functions
              #'emacs-flash-isearch--evil-after-change
              nil t)
    (add-hook 'pre-command-hook
              #'emacs-flash-isearch--evil-pre-command nil t)
    ;; Bind toggle key with highest priority using emulation-mode-map-alists
    ;; This overrides even evil state maps
    (when emacs-flash-isearch-toggle-key
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd emacs-flash-isearch-toggle-key)
                    #'emacs-flash-isearch--toggle)
        (setq emacs-flash-isearch--emulation-alist
              `((emacs-flash-isearch--in-session . ,map)))
        (unless (memq 'emacs-flash-isearch--emulation-alist
                      emulation-mode-map-alists)
          (push 'emacs-flash-isearch--emulation-alist
                emulation-mode-map-alists))))))

(defvar evil-ex-search-keymap)

(defun emacs-flash-isearch--setup-evil ()
  "Set up evil-ex-search integration."
  (advice-add 'evil-ex-search-start-session :after
              #'emacs-flash-isearch--evil-start-advice)
  (advice-add 'evil-ex-search-stop-session :before
              #'emacs-flash-isearch--evil-stop-advice)
  (add-hook 'minibuffer-setup-hook
            #'emacs-flash-isearch--evil-minibuffer-setup)
  ;; Toggle key for evil search
  (when (and emacs-flash-isearch-toggle-key
             (boundp 'evil-ex-search-keymap))
    (define-key evil-ex-search-keymap
                (kbd emacs-flash-isearch-toggle-key)
                #'emacs-flash-isearch--toggle)))

(defun emacs-flash-isearch--teardown-evil ()
  "Remove evil-ex-search integration."
  (advice-remove 'evil-ex-search-start-session
                 #'emacs-flash-isearch--evil-start-advice)
  (advice-remove 'evil-ex-search-stop-session
                 #'emacs-flash-isearch--evil-stop-advice)
  (remove-hook 'minibuffer-setup-hook
               #'emacs-flash-isearch--evil-minibuffer-setup)
  ;; Remove toggle key
  (when (and emacs-flash-isearch-toggle-key
             (boundp 'evil-ex-search-keymap))
    (define-key evil-ex-search-keymap
                (kbd emacs-flash-isearch-toggle-key) nil)))

;;; Isearch Integration

(defun emacs-flash-isearch--isearch-start ()
  "Start flash when isearch starts."
  (emacs-flash-isearch--start))

(defun emacs-flash-isearch--isearch-end ()
  "Stop flash when isearch ends."
  (emacs-flash-isearch--stop)
  ;; Perform pending jump after search cleanup
  (run-at-time 0 nil #'emacs-flash-isearch--do-pending-jump))

(defun emacs-flash-isearch--isearch-update ()
  "Update flash during isearch."
  (when (and emacs-flash-isearch--active
             (bound-and-true-p isearch-string))
    (emacs-flash-isearch--update isearch-string)))

(defun emacs-flash-isearch--printing-char-advice (orig-fun &rest args)
  "Advice for `isearch-printing-char' to intercept label keys.
ORIG-FUN is the original function, ARGS are passed through."
  (if (and emacs-flash-isearch--active
           emacs-flash-isearch--state
           (characterp last-command-event))
      (cond
       ;; Label mode is active - next char is the label
       (emacs-flash-isearch--label-mode
        (setq emacs-flash-isearch--label-mode nil)
        (if (emacs-flash-isearch--try-jump last-command-event)
            (isearch-exit)
          ;; Not a valid label, continue with normal input
          (apply orig-fun args)))
       ;; Trigger character pressed - activate label mode
       ((and emacs-flash-isearch-trigger
             (= last-command-event (string-to-char emacs-flash-isearch-trigger))
             (> (length (emacs-flash-state-matches emacs-flash-isearch--state)) 0))
        (setq emacs-flash-isearch--label-mode t)
        (message "[label?]"))
       ;; No trigger configured - try jump if label matches
       ((and (null emacs-flash-isearch-trigger)
             (emacs-flash-state-matches emacs-flash-isearch--state))
        (if (emacs-flash-isearch--try-jump last-command-event)
            (isearch-exit)
          ;; Not a valid label, continue with normal input
          (apply orig-fun args)))
       ;; Default - normal isearch behavior
       (t (apply orig-fun args)))
    ;; Flash not active - normal behavior
    (apply orig-fun args)))

(defun emacs-flash-isearch--setup-isearch ()
  "Set up isearch integration."
  (add-hook 'isearch-mode-hook #'emacs-flash-isearch--isearch-start)
  (add-hook 'isearch-mode-end-hook #'emacs-flash-isearch--isearch-end)
  (add-hook 'isearch-update-post-hook #'emacs-flash-isearch--isearch-update)
  ;; Advice to intercept label keys
  (advice-add 'isearch-printing-char :around
              #'emacs-flash-isearch--printing-char-advice)
  ;; Toggle key
  (when emacs-flash-isearch-toggle-key
    (define-key isearch-mode-map (kbd emacs-flash-isearch-toggle-key)
                #'emacs-flash-isearch--toggle)))

(defun emacs-flash-isearch--teardown-isearch ()
  "Remove isearch integration."
  (remove-hook 'isearch-mode-hook #'emacs-flash-isearch--isearch-start)
  (remove-hook 'isearch-mode-end-hook #'emacs-flash-isearch--isearch-end)
  (remove-hook 'isearch-update-post-hook #'emacs-flash-isearch--isearch-update)
  ;; Remove advice
  (advice-remove 'isearch-printing-char
                 #'emacs-flash-isearch--printing-char-advice)
  (when (and emacs-flash-isearch-toggle-key
             (boundp 'isearch-mode-map))
    (define-key isearch-mode-map (kbd emacs-flash-isearch-toggle-key) nil)))

;;; Minor Mode

;;;###autoload
(define-minor-mode emacs-flash-isearch-mode
  "Show flash labels during incremental search.
Works with both evil-ex-search and isearch."
  :global t
  :lighter " FlashS"
  :group 'emacs-flash-isearch
  (if emacs-flash-isearch-mode
      (progn
        (when (featurep 'evil)
          (emacs-flash-isearch--setup-evil))
        (emacs-flash-isearch--setup-isearch))
    (emacs-flash-isearch--stop)
    (when (featurep 'evil)
      (emacs-flash-isearch--teardown-evil))
    (emacs-flash-isearch--teardown-isearch)))

(provide 'emacs-flash-isearch)
;;; emacs-flash-isearch.el ends here
