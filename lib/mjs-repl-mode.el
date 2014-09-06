(require 'mjs-mode)

(defcustom inferior-mjs-repl-program "mjsish"
  "Path to mjs repl application."
  :type 'string
  :group 'mjs-mode)

(defcustom inferior-mjs-repl-port 8484
  "Network port to start repl evaluation server."
  :type 'number
  :group 'mjs-mode)

(defun mjs-repl ()
  (interactive)
  (pop-to-buffer (mjs-repl-make-comint))
  (mjs-repl-mode))

(defun mjs-repl-make-comint ()
  (make-comint "mjs-repl" inferior-mjs-repl-program nil
               "--no-tty" "--port" (number-to-string inferior-mjs-repl-port)))

(defun mjs-repl-eval (&optional process)
  (interactive)
  (let ((code (concat (mjs-chomp-end (mjs-region-string)) ";\n"))
        (repl (or process (mjs-repl-connection))))
    (comint-send-string repl code)))

(defun mjs-repl-connection ()
  (make-comint "mjs-repl connection" (cons "localhost" inferior-mjs-repl-port)))

(defun mjs-chomp-end (str)
  "Chomp tailing whitespace from STR."
  (replace-regexp-in-string (rx (* (any " \t\n")) eos)
                            ""
                            str))

(defun mjs-region-string ()
  (buffer-substring (region-beginning) (region-end)))


(define-derived-mode mjs-repl-mode comint-mode "Metascript REPL"
  "Major mode for Metascript REPL"
  :syntax-table mjs-mode-syntax-table
  (setq-local font-lock-defaults '(nil nil t)))


(provide 'mjs-repl-mode)
