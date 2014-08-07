(require 'ert)
(require 'mjs-mode)


(defmacro with-mjs-buffer (&rest body)
  "Similar to `with-temp-buffer' except the body is evaluated after `mjs-mode'."
  (declare (indent 0))
  `(with-temp-buffer
     (let ((mjs-mode-hook nil))
       (mjs-mode)
       ,@body)))


(defmacro let-temp-buffer (symbol &rest body)
  "Create a temporary buffer, assign it to symbol, evaluate BODY like `progn', and then kill the buffer."
  (declare (indent 1) (debug t))
  `(let ((,symbol (generate-new-buffer " *temp*")))
     (unwind-protect
         (progn ,@body)
       (and (buffer-name ,symbol)
            (kill-buffer ,symbol)))))


(defun mjs-accept-repl-output (repl)
  (accept-process-output (get-buffer-process repl) 1))


(defmacro let-temp-mjs-repl (symbol &rest body)
  "Create a temporary buffer, assign it to symbol, evaluate BODY like `progn', and then kill the buffer."
  (declare (indent 1) (debug t))
  `(let ((,symbol (mjs-repl-make-comint)))
     (set-process-query-on-exit-flag (get-buffer-process ,symbol) nil)
     (unwind-protect
         (progn
           (mjs-accept-repl-output ,symbol)
           ,@body)
       (progn
         (and (buffer-name ,symbol)
              (with-current-buffer ,symbol
                (comint-send-eof)
                t)
              (kill-buffer ,symbol))))))


(ert-deftest mjs-can-mark-sexp ()
  (with-mjs-buffer
    (let ((mjs-sexp "var f = () ->\n  42\n"))
      (insert mjs-sexp mjs-sexp)
      (goto-char (point-min))
      (mjs-mark-sexp)
      (should (equal mjs-sexp (mjs-region-string))))))


(defun test-mjs-repl-eval-after (setup)
  (let-temp-mjs-repl repl
    (with-mjs-buffer
      (insert "var a = 2 * 21")
      (funcall setup)
      (mjs-repl-eval repl)
      (mjs-accept-repl-output repl)
      (with-current-buffer repl
        (should (equal "mjs> 42\n" (buffer-substring-no-properties (point-min) (point-max))))))))


(ert-deftest mjs-repl-eval-sends-active-region-to-repl ()
  (test-mjs-repl-eval-after
   (lambda () (set-mark 9))))


(ert-deftest mjs-repl-eval-sends-enclosing-sexp-when-region-is-not-active ()
  (test-mjs-repl-eval-after
   (lambda () (goto-char (point-end)))))
