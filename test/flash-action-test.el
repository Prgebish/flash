;;; flash-action-test.el --- Tests for remote Flash actions -*- lexical-binding: t -*-

;;; Commentary:
;; ERT tests for `flash-action'.  Behavioral tests run when Evil is available.

;;; Code:

(require 'ert)
(require 'ring)
(require 'flash-action)

(declare-function evil-insert-state "evil-states")
(declare-function evil-get-register "evil-common" (register &optional noerror))
(declare-function evil-local-set-key "evil-core" (state key def))
(declare-function evil-local-mode "evil-states" (&optional arg))
(declare-function evil-normal-state "evil-states")

(ert-deftest flash-action-commands-exist-test ()
  "Public action commands are interactive."
  (should (commandp 'flash-action))
  (should (commandp 'flash-action-cancel))
  (should (commandp 'flash-action-undo)))

(defun flash-action-test--run (keys text target origin)
  "Run remote action KEYS on TEXT at TARGET, invoked from ORIGIN.
Return buffer contents, latest kill, final point, and cleanup state."
  (with-temp-buffer
    (save-window-excursion
      (set-window-buffer (selected-window) (current-buffer))
      (evil-local-mode 1)
      (evil-normal-state)
      (insert text)
      (goto-char origin)
      (setq kill-ring nil
            flash-action--last-change nil)
      (let ((session (flash-action--capture-origin)))
        (setf (flash-action--session-target-window session) (selected-window)
              (flash-action--session-target-buffer session) (current-buffer)
              (flash-action--session-target-point session)
              (flash-action--marker (current-buffer) target))
        (flash-action--arm session)
        (execute-kbd-macro (kbd keys))
        (list (buffer-string) (car kill-ring) (point)
              flash-action--session flash-action--exit-transient)))))

(when (require 'evil nil t)
  (ert-deftest flash-action-native-evil-grammar-test ()
    "Native Evil operators, text objects, and counts run at the target."
    (let ((cases '(("yy" "alpha beta\nsecond line\n" 1 20
                    "alpha beta\nsecond line\n" "alpha beta\n")
                   ("dd" "alpha beta\nsecond line\n" 1 20
                    "second line\n" "alpha beta\n")
                   ("yiw" "alpha beta\n" 3 10
                    "alpha beta\n" "alpha")
                   ("diw" "alpha beta\n" 3 10
                    " beta\n" "alpha")
                   ("y$" "alpha beta\n" 7 10
                    "alpha beta\n" "beta")
                   ("d$" "alpha beta\n" 7 10
                    "alpha \n" "beta")
                   ("yf." "alpha.beta\n" 1 10
                    "alpha.beta\n" "alpha.")
                   ("df." "alpha.beta\n" 1 10
                    "beta\n" "alpha.")
                   ("d2w" "one two three four\n" 1 18
                    "three four\n" "one two ")
                   ("2dw" "one two three four\n" 1 18
                    "three four\n" "one two "))))
      (dolist (case cases)
        (pcase-let ((`(,keys ,text ,target ,origin ,expected ,killed) case))
          (let ((result (flash-action-test--run keys text target origin)))
            (should (equal (nth 0 result) expected))
            (should (equal (nth 1 result) killed))
            (should-not (nth 3 result))
            (should-not (nth 4 result)))))))

  (ert-deftest flash-action-direct-commands-test ()
    "Direct Y, D, x, and X commands keep their native Evil semantics."
    (let ((y-result (flash-action-test--run
                     "Y" "alpha beta\nsecond\n" 3 15))
          (d-result (flash-action-test--run
                     "D" "alpha beta\nsecond\n" 7 15))
          (x-result (flash-action-test--run
                     "x" "alpha beta\n" 2 10))
          (backward-result (flash-action-test--run
                            "X" "alpha beta\n" 3 10)))
      (should (equal (nth 1 y-result) "alpha beta\n"))
      (should (equal (nth 0 d-result) "alpha \nsecond\n"))
      (should (equal (nth 0 x-result) "apha beta\n"))
      (should (equal (nth 0 backward-result) "apha beta\n"))))

  (ert-deftest flash-action-register-prefix-test ()
    "Register prefixes remain available before the remote operator."
    (with-temp-buffer
      (save-window-excursion
        (set-window-buffer (selected-window) (current-buffer))
        (evil-local-mode 1)
        (evil-normal-state)
        (insert "alpha beta\n")
        (goto-char (point-max))
        (let ((session (flash-action--capture-origin)))
          (setf (flash-action--session-target-window session) (selected-window)
                (flash-action--session-target-buffer session) (current-buffer)
                (flash-action--session-target-point session)
                (flash-action--marker (current-buffer) 1))
          (flash-action--arm session)
          (execute-kbd-macro (kbd "\"ayy"))
          (should (equal (evil-get-register ?a) "alpha beta\n"))))))

  (ert-deftest flash-action-does-not-replace-dot-repeat-test ()
    "The complete gS flow does not replace the previous dot command."
    (with-temp-buffer
      (save-window-excursion
        (set-window-buffer (selected-window) (current-buffer))
        (evil-local-mode 1)
        (evil-normal-state)
        (evil-local-set-key 'normal (kbd "g S") #'flash-action)
        (insert "alpha beta\norigin line\n")
        (goto-char (point-min))
        (forward-line 1)
        (let ((evil-repeat-ring (make-ring 10))
              (flash-multi-window nil)
              (flash-autojump nil)
              (flash-min-pattern-length 1))
          (ring-insert evil-repeat-ring 'previous-repeat)
          (execute-kbd-macro (kbd "g S a l p h a RET d d"))
          (should (eq (ring-ref evil-repeat-ring 0)
                      'previous-repeat))))))

  (ert-deftest flash-action-restores-other-window-test ()
    "A cross-window action restores the invoking window and point."
    (let ((origin-buffer (generate-new-buffer " *flash-action-origin*"))
          (target-buffer (generate-new-buffer " *flash-action-target*")))
      (unwind-protect
          (save-window-excursion
            (delete-other-windows)
            (let* ((origin-window (selected-window))
                   (target-window (split-window-right)))
              (set-window-buffer origin-window origin-buffer)
              (set-window-buffer target-window target-buffer)
              (select-window origin-window)
              (with-current-buffer origin-buffer
                (insert "origin text")
                (goto-char 5)
                (evil-local-mode 1)
                (evil-normal-state))
              (with-current-buffer target-buffer
                (insert "alpha beta\nsecond\n")
                (evil-local-mode 1)
                (evil-normal-state))
              (let ((session (flash-action--capture-origin)))
                (setf (flash-action--session-target-window session) target-window
                      (flash-action--session-target-buffer session) target-buffer
                      (flash-action--session-target-point session)
                      (flash-action--marker target-buffer 1))
                (flash-action--arm session)
                (execute-kbd-macro (kbd "dd")))
              (should (eq (selected-window) origin-window))
              (should (eq (current-buffer) origin-buffer))
              (should (= (point) 5))
              (should (equal (with-current-buffer target-buffer (buffer-string))
                             "second\n"))))
        (kill-buffer origin-buffer)
        (kill-buffer target-buffer))))

  (ert-deftest flash-action-public-flow-has-no-jump-side-effects-test ()
    "The public flow targets match start without using normal jump behavior."
    (with-temp-buffer
      (save-window-excursion
        (set-window-buffer (selected-window) (current-buffer))
        (evil-local-mode 1)
        (evil-normal-state)
        (insert "alpha beta\n")
        (goto-char (point-max))
        (push-mark 4 t t)
        (let* ((origin (point))
               (origin-mark (mark))
               (hook-calls 0)
               (flash-jump-position 'end)
               (flash-jumplist t)
               (flash-after-jump-hook
                (list (lambda () (setq hook-calls (1+ hook-calls)))))
               (match (make-flash-match
                       :pos 1 :end-pos 6 :buffer (current-buffer)
                       :window (selected-window))))
          (cl-letf (((symbol-function 'flash--loop)
                     (lambda (_state) match)))
            (flash-action))
          (should (= (point) 1))
          (execute-kbd-macro (kbd "x"))
          (should (equal (buffer-string) "lpha beta\n"))
          (should (= (point) (1- origin)))
          (should (= (mark) 3))
          (should (= origin-mark 4))
          (should (zerop hook-calls))))))

  (ert-deftest flash-action-end-to-end-key-sequence-test ()
    "A complete gS, search, RET, and dd sequence works through the command loop."
    (with-temp-buffer
      (save-window-excursion
        (set-window-buffer (selected-window) (current-buffer))
        (evil-local-mode 1)
        (evil-normal-state)
        (evil-local-set-key 'normal (kbd "g S") #'flash-action)
        (insert "alpha beta\norigin line\n")
        (goto-char (point-min))
        (forward-line 1)
        (let ((flash-multi-window nil)
              (flash-autojump nil)
              (flash-min-pattern-length 1))
          (execute-kbd-macro (kbd "g S a l p h a RET d d")))
        (should (equal (buffer-string) "origin line\n"))
        (should (= (point) (point-min)))
        (should-not flash-action--session)
        (should-not flash-action--exit-transient))))

  (ert-deftest flash-action-cancel-and-invalid-key-test ()
    "Cancel and unsupported keys restore origin without leaking a command."
    (let ((cancelled (flash-action-test--run
                      "ESC" "alpha beta\n" 1 8)))
      (should (equal (nth 0 cancelled) "alpha beta\n"))
      (should (= (nth 2 cancelled) 8))
      (should-not (nth 3 cancelled)))
    (should-error
     (flash-action-test--run "p" "alpha beta\n" 1 8)
     :type 'user-error))

  (ert-deftest flash-action-read-only-error-restores-test ()
    "An operator error still removes the map and restores point."
    (with-temp-buffer
      (save-window-excursion
        (set-window-buffer (selected-window) (current-buffer))
        (evil-local-mode 1)
        (evil-normal-state)
        (insert "alpha beta\n")
        (goto-char 8)
        (let ((session (flash-action--capture-origin)))
          (setf (flash-action--session-target-window session) (selected-window)
                (flash-action--session-target-buffer session) (current-buffer)
                (flash-action--session-target-point session)
                (flash-action--marker (current-buffer) 1))
          (flash-action--arm session)
          (setq buffer-read-only t)
          (should-error (execute-kbd-macro (kbd "dd"))
                        :type 'buffer-read-only)
          (should (= (point) 8))
          (should-not flash-action--session)
          (should-not flash-action--exit-transient)))))

  (ert-deftest flash-action-dead-target-restores-test ()
    "A target killed while waiting produces an error and restores origin."
    (let ((origin-buffer (generate-new-buffer " *flash-action-origin*"))
          (target-buffer (generate-new-buffer " *flash-action-target*")))
      (unwind-protect
          (save-window-excursion
            (delete-other-windows)
            (let* ((origin-window (selected-window))
                   (target-window (split-window-right)))
              (set-window-buffer origin-window origin-buffer)
              (set-window-buffer target-window target-buffer)
              (with-current-buffer origin-buffer
                (insert "origin")
                (goto-char 4)
                (evil-local-mode 1)
                (evil-normal-state))
              (with-current-buffer target-buffer
                (insert "target")
                (evil-local-mode 1)
                (evil-normal-state))
              (select-window origin-window)
              (let ((session (flash-action--capture-origin)))
                (setf (flash-action--session-target-window session) target-window
                      (flash-action--session-target-buffer session) target-buffer
                      (flash-action--session-target-point session)
                      (flash-action--marker target-buffer 1))
                (flash-action--arm session)
                (kill-buffer target-buffer)
                (should-error (execute-kbd-macro (kbd "x"))
                              :type 'user-error))
              (should (eq (selected-window) origin-window))
              (should (eq (current-buffer) origin-buffer))
              (should (= (point) 4))
              (should-not flash-action--session)
              (should-not flash-action--exit-transient)))
        (when (buffer-live-p origin-buffer)
          (kill-buffer origin-buffer))
        (when (buffer-live-p target-buffer)
          (kill-buffer target-buffer)))))

  (ert-deftest flash-action-undo-remote-change-test ()
    "Remote undo reverses the recorded destructive operation."
    (with-temp-buffer
      (save-window-excursion
        (set-window-buffer (selected-window) (current-buffer))
        (evil-local-mode 1)
        (evil-normal-state)
        (buffer-enable-undo)
        (insert "alpha beta\nsecond\n")
        (setq buffer-undo-list nil)
        (goto-char (point-max))
        (let ((session (flash-action--capture-origin)))
          (setf (flash-action--session-target-window session) (selected-window)
                (flash-action--session-target-buffer session) (current-buffer)
                (flash-action--session-target-point session)
                (flash-action--marker (current-buffer) 1))
          (flash-action--arm session)
          (execute-kbd-macro (kbd "dd"))
          (should (equal (buffer-string) "second\n"))
          (flash-action-undo)
          (should (equal (buffer-string) "alpha beta\nsecond\n"))
          (should-not flash-action--last-change)))))

  (ert-deftest flash-action-requires-normal-state-test ()
    "The public command refuses Evil insert state before starting Flash."
    (with-temp-buffer
      (evil-local-mode 1)
      (evil-insert-state)
      (should-error (flash-action) :type 'user-error))))

(provide 'flash-action-test)
;;; flash-action-test.el ends here
