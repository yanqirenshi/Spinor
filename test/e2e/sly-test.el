;;; sly-test.el --- E2E tests for Spinor SLY contribs -*- lexical-binding: t -*-

;;; Commentary:
;; Run these tests with Spinor server running:
;;   1. Start server: cabal run spinor
;;   2. Connect SLY: M-x sly-connect RET localhost RET 4005
;;   3. Load this file: M-x load-file RET test/e2e/sly-test.el
;;   4. Run tests: M-x spinor-run-e2e-tests

;;; Code:

(require 'sly)

(defvar spinor-e2e-test-results nil
  "Results of E2E tests.")

(defun spinor-e2e-log (format-string &rest args)
  "Log test message with FORMAT-STRING and ARGS."
  (let ((msg (apply #'format format-string args)))
    (message "[Spinor E2E] %s" msg)
    msg))

(defmacro spinor-e2e-test (name &rest body)
  "Define an E2E test with NAME and BODY."
  (declare (indent 1))
  `(condition-case err
       (progn
         (spinor-e2e-log "Running: %s" ,name)
         ,@body
         (push (cons ,name 'pass) spinor-e2e-test-results)
         (spinor-e2e-log "PASS: %s" ,name))
     (error
      (push (cons ,name (format "FAIL: %s" (error-message-string err)))
            spinor-e2e-test-results)
      (spinor-e2e-log "FAIL: %s - %s" ,name (error-message-string err)))))

(defun spinor-e2e-assert (condition message)
  "Assert CONDITION is true, signal error with MESSAGE if not."
  (unless condition
    (error "Assertion failed: %s" message)))

;;; Tests

(defun spinor-e2e-test-connection ()
  "Test basic SLY connection."
  (spinor-e2e-test "Connection"
    (spinor-e2e-assert (sly-connected-p) "SLY should be connected")))

(defun spinor-e2e-test-eval ()
  "Test basic evaluation."
  (spinor-e2e-test "Basic eval (+ 1 2)"
    (let ((result (sly-eval '(slynk:eval-and-grab-output "(+ 1 2)"))))
      (spinor-e2e-assert (stringp (cadr result)) "Should return string result")
      (spinor-e2e-assert (string-match "3" (cadr result)) "Result should contain 3"))))

(defun spinor-e2e-test-completions ()
  "Test completion functionality."
  (spinor-e2e-test "Completions for 'str'"
    (let ((result (sly-eval '(slynk-completion:flex-completions "str" "SPINOR"))))
      (spinor-e2e-assert (listp result) "Should return a list")
      (spinor-e2e-assert (listp (car result)) "First element should be completion list"))))

(defun spinor-e2e-test-autodoc ()
  "Test autodoc functionality."
  (spinor-e2e-test "Autodoc for cons"
    (let ((result (sly-eval '(slynk:autodoc '("cons" slynk::%cursor-marker%) :print-right-margin 80))))
      (spinor-e2e-assert (listp result) "Should return a list"))))

(defun spinor-e2e-test-trace-dialog ()
  "Test trace dialog functionality."
  (spinor-e2e-test "Trace toggle"
    ;; Toggle trace on
    (let ((result1 (sly-eval '(slynk-trace-dialog:dialog-toggle-trace
                               (slynk::from-string "test-func")))))
      (spinor-e2e-assert (stringp result1) "Toggle should return string"))
    ;; Toggle trace off
    (let ((result2 (sly-eval '(slynk-trace-dialog:dialog-toggle-trace
                               (slynk::from-string "test-func")))))
      (spinor-e2e-assert (stringp result2) "Toggle again should return string"))))

(defun spinor-e2e-test-report-specs ()
  "Test trace report-specs."
  (spinor-e2e-test "Report specs"
    (let ((result (sly-eval '(slynk-trace-dialog:report-specs))))
      (spinor-e2e-assert (listp result) "Should return a list"))))

(defun spinor-e2e-test-inspector ()
  "Test inspector functionality."
  (spinor-e2e-test "Inspector init"
    (let ((result (sly-eval '(slynk:eval-for-inspector
                              nil nil
                              'slynk:init-inspector
                              "42"))))
      (spinor-e2e-assert (listp result) "Should return plist")
      ;; Check for :title key
      (spinor-e2e-assert (plist-get result :title) "Should have :title"))))

(defun spinor-e2e-test-stickers ()
  "Test stickers functionality."
  (spinor-e2e-test "Stickers total-recordings"
    (let ((result (sly-eval '(slynk-stickers:total-recordings))))
      (spinor-e2e-assert (integerp result) "Should return integer")
      (spinor-e2e-assert (= result 0) "Should be 0 (no recordings)")))

  (spinor-e2e-test "Stickers fetch"
    (let ((result (sly-eval '(slynk-stickers:fetch nil))))
      (spinor-e2e-assert (listp result) "Should return list")))

  (spinor-e2e-test "Stickers forget"
    (let ((result (sly-eval '(slynk-stickers:forget nil))))
      (spinor-e2e-assert (integerp result) "Should return integer"))))

;;; Test Runner

(defun spinor-run-e2e-tests ()
  "Run all Spinor E2E tests."
  (interactive)
  (setq spinor-e2e-test-results nil)

  (unless (sly-connected-p)
    (error "SLY is not connected. Connect first with M-x sly-connect"))

  (spinor-e2e-log "Starting E2E tests...")
  (spinor-e2e-log "================================")

  ;; Run tests
  (spinor-e2e-test-connection)
  (spinor-e2e-test-eval)
  (spinor-e2e-test-completions)
  (spinor-e2e-test-autodoc)
  (spinor-e2e-test-trace-dialog)
  (spinor-e2e-test-report-specs)
  (spinor-e2e-test-inspector)
  (spinor-e2e-test-stickers)

  ;; Summary
  (spinor-e2e-log "================================")
  (let ((passed (cl-count 'pass spinor-e2e-test-results :key #'cdr))
        (total (length spinor-e2e-test-results)))
    (spinor-e2e-log "Results: %d/%d passed" passed total)
    (dolist (result (reverse spinor-e2e-test-results))
      (if (eq (cdr result) 'pass)
          (spinor-e2e-log "  [PASS] %s" (car result))
        (spinor-e2e-log "  [FAIL] %s: %s" (car result) (cdr result)))))

  spinor-e2e-test-results)

(provide 'sly-test)
;;; sly-test.el ends here
