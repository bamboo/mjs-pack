(defun mjs-lib-file (filename)
  (concat live-current-pack-dir "lib/" filename ".el"))

(autoload 'mjs-mode (mjs-lib-file "mjs-mode") "Metascript Mode." t)

(add-to-list 'auto-mode-alist '("\\.mjs\\'" . mjs-mode))

(let ((mjs-repl-mode-file (mjs-lib-file "mjs-repl-mode")))
  (autoload 'mjs-repl mjs-repl-mode-file "Metascript REPL" t)
  (autoload 'mjs-repl-eval mjs-repl-mode-file))
