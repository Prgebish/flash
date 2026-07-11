;;; flash-action.el --- Remote Evil actions for Flash -*- lexical-binding: t -*-

;; Copyright (C) 2025 Vadim Pavlov
;; Author: Vadim Pavlov <https://github.com/Prgebish>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Run a yank or delete command at a target selected with Flash, then return
;; to the original window and position.  Evil remains an optional dependency:
;; this module can be loaded without Evil, but `flash-action' requires it.

;;; Code:

(require 'cl-lib)
(require 'flash)

(declare-function evil-change-state "evil-states" (state))
(declare-function evil-add-command-properties "evil-common" (command &rest properties))
(declare-function evil-delete "evil-commands")
(declare-function evil-delete-backward-char "evil-commands")
(declare-function evil-delete-char "evil-commands")
(declare-function evil-delete-line "evil-commands")
(declare-function evil-undo "evil-commands" (count))
(declare-function evil-use-register "evil-commands")
(declare-function evil-yank "evil-commands")
(declare-function evil-yank-line "evil-commands")

(defvar evil-local-mode)
(defvar evil-state)

(cl-defstruct (flash-action--session
               (:constructor flash-action--make-session))
  "State kept while a remote action is pending."
  origin-window
  origin-buffer
  origin-point
  origin-mark
  origin-mark-active
  origin-evil-state
  target-window
  target-buffer
  target-point
  stage)

(cl-defstruct (flash-action--change
               (:constructor flash-action--make-change))
  "A remote change that can be undone safely."
  buffer
  tick)

(defvar flash-action--session nil
  "Current pending remote action session, or nil.")

(defvar flash-action--exit-transient nil
  "Function that deactivates the current remote action keymap.")

(defvar flash-action--last-change nil
  "Most recent remote destructive action available for undo.")

(defvar flash-action--map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "y") #'flash-action--yank)
    (define-key map (kbd "d") #'flash-action--delete)
    (define-key map (kbd "Y") #'flash-action--yank-line)
    (define-key map (kbd "D") #'flash-action--delete-line)
    (define-key map (kbd "x") #'flash-action--delete-char)
    (define-key map (kbd "X") #'flash-action--delete-backward-char)
    (define-key map (kbd "\"") #'evil-use-register)
    (dotimes (digit 10)
      (define-key map (number-to-string digit) #'digit-argument))
    (define-key map (kbd "ESC") #'flash-action-cancel)
    (define-key map (kbd "C-g") #'flash-action-cancel)
    (define-key map [t] #'flash-action--invalid-key)
    map)
  "Transient keymap used after a Flash target has been selected.")

(defun flash-action--marker (buffer position &optional insertion-type)
  "Make a marker in BUFFER at POSITION with INSERTION-TYPE.
Return nil when POSITION is nil."
  (when position
    (let ((marker (make-marker)))
      (set-marker marker position buffer)
      (set-marker-insertion-type marker insertion-type)
      marker)))

(defun flash-action--capture-origin ()
  "Create a remote action session from the current editor state."
  (flash-action--make-session
   :origin-window (selected-window)
   :origin-buffer (current-buffer)
   :origin-point (flash-action--marker (current-buffer) (point))
   :origin-mark (flash-action--marker (current-buffer) (mark t))
   :origin-mark-active mark-active
   :origin-evil-state (and (boundp 'evil-state) evil-state)
   :stage 'selecting))

(defun flash-action--release-marker (marker)
  "Detach MARKER when it is a marker."
  (when (markerp marker)
    (set-marker marker nil)))

(defun flash-action--release-session (session)
  "Release all markers owned by SESSION."
  (flash-action--release-marker
   (flash-action--session-origin-point session))
  (flash-action--release-marker
   (flash-action--session-origin-mark session))
  (flash-action--release-marker
   (flash-action--session-target-point session)))

(defun flash-action--select-buffer-window (buffer preferred-window)
  "Select a window displaying BUFFER, preferring PREFERRED-WINDOW.
When necessary, display BUFFER in a non-dedicated selected window.
Return the selected window, or nil when BUFFER is no longer live."
  (when (buffer-live-p buffer)
    (let ((window (cond
                   ((and (window-live-p preferred-window)
                         (eq (window-buffer preferred-window) buffer))
                    preferred-window)
                   ((get-buffer-window buffer t))
                   ((and (window-live-p preferred-window)
                         (not (window-dedicated-p preferred-window)))
                    (set-window-buffer preferred-window buffer)
                    preferred-window)
                   ((not (window-dedicated-p (selected-window)))
                    (set-window-buffer (selected-window) buffer)
                    (selected-window)))))
      (when (window-live-p window)
        (select-window window)
        (set-buffer (window-buffer window))
        window))))

(defun flash-action--goto-target (session)
  "Select SESSION's target and move point there.
Return non-nil when the target is still usable."
  (let ((marker (flash-action--session-target-point session))
        (buffer (flash-action--session-target-buffer session)))
    (when (and (markerp marker)
               (marker-position marker)
               (eq (marker-buffer marker) buffer)
               (flash-action--select-buffer-window
                buffer (flash-action--session-target-window session)))
      (goto-char marker)
      t)))

(defun flash-action--restore-origin (session)
  "Restore the editor state saved in SESSION."
  (let ((buffer (flash-action--session-origin-buffer session))
        (point-marker (flash-action--session-origin-point session))
        (mark-marker (flash-action--session-origin-mark session)))
    (when (and (buffer-live-p buffer)
               (flash-action--select-buffer-window
                buffer (flash-action--session-origin-window session)))
      (when (and (markerp point-marker) (marker-position point-marker))
        (goto-char point-marker))
      (if (and (markerp mark-marker) (marker-position mark-marker))
          (set-marker (mark-marker) (marker-position mark-marker) buffer)
        (set-marker (mark-marker) nil))
      (when (and (bound-and-true-p evil-local-mode)
                 (flash-action--session-origin-evil-state session)
                 (fboundp 'evil-change-state))
        (evil-change-state
         (flash-action--session-origin-evil-state session)))
      (setq mark-active
            (flash-action--session-origin-mark-active session)))))

(defun flash-action--finish ()
  "Deactivate the action keymap, restore origin, and release session state."
  (let ((session flash-action--session)
        (exit flash-action--exit-transient))
    (setq flash-action--session nil
          flash-action--exit-transient nil)
    (when exit
      (funcall exit))
    (when session
      (when (buffer-live-p (flash-action--session-target-buffer session))
        (with-current-buffer (flash-action--session-target-buffer session)
          (remove-hook 'post-command-hook
                       #'flash-action--post-command-finish t)))
      (unwind-protect
          (flash-action--restore-origin session)
        (flash-action--release-session session)))))

(defun flash-action--post-command-finish ()
  "Restore origin after Evil has completed its own post-command work."
  (when (and flash-action--session
             (eq (flash-action--session-stage flash-action--session)
                 'restoring))
    (flash-action--finish)))

(defun flash-action--schedule-finish (session)
  "Arrange to restore SESSION after the current command completes."
  (setf (flash-action--session-stage session) 'restoring)
  (add-hook 'post-command-hook #'flash-action--post-command-finish t t))

(defun flash-action--transient-exited ()
  "Cancel a pending action when its transient keymap exits unexpectedly."
  (when (and flash-action--session
             (eq (flash-action--session-stage flash-action--session)
                 'pending))
    (flash-action--finish)))

(defun flash-action--arm (session)
  "Move to SESSION's target and wait for one supported Evil action."
  (unless (flash-action--goto-target session)
    (flash-action--release-session session)
    (user-error "Flash action target is no longer available"))
  (setf (flash-action--session-stage session) 'pending)
  (setq flash-action--session session
        flash-action--exit-transient
        (set-transient-map flash-action--map t
                           #'flash-action--transient-exited))
  (message "Flash action: y/d/Y/D/x/X (ESC cancels)"))

(defun flash-action--snapshot-target (session match)
  "Store MATCH as SESSION's target before Flash state cleanup."
  (let ((buffer (flash-match-buffer-live match))
        (position (flash-match-pos-value match)))
    (when (and (buffer-live-p buffer) (integer-or-marker-p position))
      (setf (flash-action--session-target-buffer session) buffer
            (flash-action--session-target-window session)
            (flash-match-window match)
            (flash-action--session-target-point session)
            (flash-action--marker buffer position))
      t)))

;;;###autoload
(defun flash-action ()
  "Select a Flash target, run a remote Evil yank/delete, and return.
After selecting the target, use normal Evil yank/delete grammar such as
`yy', `dd', `yiw', `diw', `y$', or `d$'.  Direct `Y', `D', `x', and `X'
commands are also supported.  Press ESC or C-g to cancel."
  (interactive)
  (unless (require 'evil nil t)
    (user-error "Flash action requires the evil package"))
  (unless (and (bound-and-true-p evil-local-mode)
               (eq evil-state 'normal))
    (user-error "Flash action requires Evil normal state"))
  (when flash-action--session
    (flash-action--finish))
  (let ((session (flash-action--capture-origin))
        (state (flash--create-state))
        match)
    (unwind-protect
        (setq match (flash--loop state))
      (when match
        (flash-action--snapshot-target session match))
      (flash-state-cleanup state))
    (if (flash-action--session-target-point session)
        (flash-action--arm session)
      (flash-action--restore-origin session)
      (flash-action--release-session session))))

(defun flash-action--deactivate-map (session)
  "Deactivate the transient keymap before Evil operates on SESSION."
  (setf (flash-action--session-stage session) 'executing)
  (let ((exit flash-action--exit-transient))
    (setq flash-action--exit-transient nil)
    (when exit
      (funcall exit))))

(defun flash-action--execute (command changes-buffer)
  "Run Evil COMMAND remotely.
When CHANGES-BUFFER is non-nil, make the operation independently undoable."
  (let ((session flash-action--session)
        changed-buffer
        before-tick
        finish-scheduled)
    (unless session
      (user-error "No Flash action is pending"))
    (flash-action--deactivate-map session)
    (unwind-protect
        (progn
          (unless (flash-action--goto-target session)
            (user-error "Flash action target is no longer available"))
          (setq changed-buffer (current-buffer)
                before-tick (buffer-chars-modified-tick))
          (when changes-buffer
            (undo-boundary))
          (unwind-protect
              (let ((this-command command)
                    (real-this-command command))
                (call-interactively command))
            (when changes-buffer
              (undo-boundary)
              (when (/= before-tick (buffer-chars-modified-tick))
                (setq flash-action--last-change
                      (flash-action--make-change
                       :buffer changed-buffer
                       :tick (buffer-chars-modified-tick))))))
          (flash-action--schedule-finish session)
          (setq finish-scheduled t))
      (unless finish-scheduled
        (flash-action--finish)))))

(defun flash-action--yank ()
  "Run a remote Evil yank operator."
  (interactive)
  (flash-action--execute #'evil-yank nil))

(defun flash-action--delete ()
  "Run a remote Evil delete operator."
  (interactive)
  (flash-action--execute #'evil-delete t))

(defun flash-action--yank-line ()
  "Run a remote Evil line yank."
  (interactive)
  (flash-action--execute #'evil-yank-line nil))

(defun flash-action--delete-line ()
  "Run a remote Evil delete-to-end-of-line command."
  (interactive)
  (flash-action--execute #'evil-delete-line t))

(defun flash-action--delete-char ()
  "Run a remote Evil delete-character command."
  (interactive)
  (flash-action--execute #'evil-delete-char t))

(defun flash-action--delete-backward-char ()
  "Run a remote Evil backward-delete-character command."
  (interactive)
  (flash-action--execute #'evil-delete-backward-char t))

;;;###autoload
(defun flash-action-cancel ()
  "Cancel a pending remote action and restore the original location."
  (interactive)
  (unless flash-action--session
    (user-error "No Flash action is pending"))
  (flash-action--finish)
  (message "Flash action cancelled"))

(defun flash-action--invalid-key ()
  "Cancel a remote action instead of running an unsupported key."
  (interactive)
  (let ((key (key-description (this-command-keys-vector))))
    (flash-action--finish)
    (user-error "Unsupported Flash action: %s" key)))

(defun flash-action--set-evil-properties ()
  "Keep remote action wrappers out of Evil's dot-repeat history."
  (dolist (command '(flash-action
                     flash-action--yank
                     flash-action--delete
                     flash-action--yank-line
                     flash-action--delete-line
                     flash-action--delete-char
                     flash-action--delete-backward-char))
    (evil-add-command-properties command :repeat 'ignore)))

(with-eval-after-load 'evil
  (flash-action--set-evil-properties))

;;;###autoload
(defun flash-action-undo ()
  "Undo the most recent remote destructive Flash action.
This is useful when the action changed a buffer other than the buffer from
which `flash-action' was invoked.  Refuse to undo if that buffer has changed
again, so an unrelated edit is never undone accidentally."
  (interactive)
  (unless (require 'evil nil t)
    (user-error "Flash action undo requires the evil package"))
  (unless flash-action--last-change
    (user-error "No remote Flash action to undo"))
  (let* ((change flash-action--last-change)
         (buffer (flash-action--change-buffer change)))
    (unless (buffer-live-p buffer)
      (setq flash-action--last-change nil)
      (user-error "The changed buffer no longer exists"))
    (with-current-buffer buffer
      (save-mark-and-excursion
        (unless (= (flash-action--change-tick change)
                   (buffer-chars-modified-tick))
          (user-error "The changed buffer was edited after the Flash action"))
        (evil-undo 1)))
    (setq flash-action--last-change nil)
    (message "Undid remote Flash action in %s" (buffer-name buffer))))

(provide 'flash-action)
;;; flash-action.el ends here
