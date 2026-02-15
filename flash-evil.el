;;; flash-evil.el --- Evil integration for flash -*- lexical-binding: t -*-

;; Copyright (C) 2025 Vadim Pavlov
;; Author: Vadim Pavlov <https://github.com/Prgebish>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Evil-mode integration for flash.
;; Provides motions that work with evil operators (d, y, c, etc.)
;;
;; Usage:
;;   (require 'flash-evil)
;;   (define-key evil-normal-state-map (kbd "s") #'flash-evil-jump)
;;
;; Or use the setup function:
;;   (flash-evil-setup)

;;; Code:

(require 'flash)
(require 'flash-char)

;;; Forward declarations for optional evil dependency

(declare-function evil-global-set-key "evil")

;;; Evil Motion

;;;###autoload
(defun flash-evil-jump (&optional _count)
  "Flash jump as evil motion.
Works with operators: d s <target> deletes to target.
In visual mode: extends selection to target."
  (interactive "p")
  (flash-jump))

(defun flash-evil--define-motion ()
  "Define `flash-evil-jump' as an evil motion."
  (eval
   '(evil-define-motion flash-evil-jump (count)
      "Flash jump as evil motion.
Works with operators: d s <target> deletes to target.
In visual mode: extends selection to target."
      :type inclusive
      :jump t
      (ignore count)
      (flash-jump))))

(when (require 'evil nil t)
  (flash-evil--define-motion))

;;; Setup function

;;;###autoload
(defun flash-evil-setup (&optional char-motions)
  "Set up evil keybindings for flash.
Binds `gs' in normal, visual, motion, and operator states.
When CHAR-MOTIONS is non-nil, also replace f/t/F/T with flash versions."
  (interactive "P")
  (unless (require 'evil nil t)
    (user-error "Flash-evil requires the evil package"))
  (flash-evil--define-motion)
  ;; Flash jump on gs
  (evil-global-set-key 'normal (kbd "g s") #'flash-evil-jump)
  (evil-global-set-key 'visual (kbd "g s") #'flash-evil-jump)
  (evil-global-set-key 'motion (kbd "g s") #'flash-evil-jump)
  (evil-global-set-key 'operator (kbd "g s") #'flash-evil-jump)
  ;; Char motions
  (when char-motions
    (flash-char-setup-evil-keys))
  (message "Flash-evil: bound 'gs' to flash jump%s"
           (if char-motions ", f/t/F/T to flash char" "")))

(provide 'flash-evil)
;;; flash-evil.el ends here
