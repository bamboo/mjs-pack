(defvar mjs-mode-file (concat live-current-pack-dir "lib/mjs-mode.el"))

(autoload 'mjs-mode mjs-mode-file "Metascript Mode." t)

(add-to-list 'auto-mode-alist '("\\.mjs\\'" . mjs-mode))
