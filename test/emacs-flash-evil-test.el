;;; emacs-flash-evil-test.el --- Tests for emacs-flash-evil -*- lexical-binding: t -*-

;;; Commentary:
;; ERT tests for emacs-flash-evil module.
;; These tests require evil-mode to be available.

;;; Code:

(require 'ert)

;; Only run tests if evil is available
(when (require 'evil nil t)
  (require 'emacs-flash)
  (require 'emacs-flash-evil)

  (ert-deftest emacs-flash-evil-jump-defined-test ()
    "Test that emacs-flash-evil-jump is defined as evil motion."
    (should (fboundp 'emacs-flash-evil-jump))
    (should (evil-get-command-property 'emacs-flash-evil-jump :type)))

  (ert-deftest emacs-flash-evil-jump-type-test ()
    "Test that emacs-flash-evil-jump has inclusive type."
    (should (eq 'inclusive
                (evil-get-command-property 'emacs-flash-evil-jump :type))))

  (ert-deftest emacs-flash-evil-jump-is-jump-test ()
    "Test that emacs-flash-evil-jump is marked as jump."
    (should (evil-get-command-property 'emacs-flash-evil-jump :jump)))

  (ert-deftest emacs-flash-evil-select-defined-test ()
    "Test that emacs-flash-evil-select is defined."
    (should (fboundp 'emacs-flash-evil-select))
    (should (commandp 'emacs-flash-evil-select)))

  (ert-deftest emacs-flash-evil-setup-defined-test ()
    "Test that emacs-flash-evil-setup is defined."
    (should (fboundp 'emacs-flash-evil-setup))
    (should (commandp 'emacs-flash-evil-setup)))

)

(provide 'emacs-flash-evil-test)
;;; emacs-flash-evil-test.el ends here
