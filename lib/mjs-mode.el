;; mjs-mode.el --- Towards a decent Metascript editing experience in Emacs.

;; Keywords: languages, metascript, mjs

;; Copyright (C) 2014 Rodrigo B. de Oliveira <rbo@acm.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Installation:
;; git clone into your ~/.live-packs directory and add
;;  (live-add-packs '(~/.live-packs/mjs-pack))
;; to your ~/.emacs-live.el


(require 'rx)

(defconst mjs-mode-version "0.1"
  "`mjs-mode' version number.")

(defgroup mjs-mode nil
  "Support for the Metascript programming language."
  :group 'languages
  :prefix "mjs-")

(defcustom mjs-indent-tabs-mode nil
  "mjs-mode starts `indent-tabs-mode' with the value specified here, default is nil. "
  :type 'boolean
  :group 'mjs-mode)

(defcustom mjs-indent-offset 2
  "Amount of offset per level of indentation."
  :type 'integer
  :group 'mjs-mode)
(make-variable-buffer-local 'mjs-indent-offset)

(defcustom mjs-mode-hook nil
  "List of functions to be executed on entry to mjs-mode."
  :type 'hook
  :group 'mjs-mode)

(defmacro defmjsface (name base description)
  "Defines a mjs-mode font-lock face."
  `(progn
     (defface ,name
       '((t (:inherit ,base)))
       ,description
       :group 'mjs-mode)
     (defvar ,name ',name)))

(defmjsface mjs-control-flow-face font-lock-keyword-face
  "Highlight control flow.")

(defmjsface mjs-def-face font-lock-keyword-face
  "Highlight definitions.")

(defmjsface mjs-modifier-face font-lock-keyword-face
  "Highlight modifiers.")

(defmjsface mjs-builtin-face font-lock-builtin-face
  "Highlight builtins.")

(defmjsface mjs-constant-face font-lock-constant-face
  "Highlight constants.")

(defmjsface mjs-number-face font-lock-variable-name-face
  "Highlight numbers.")

(defmjsface mjs-type-name-face font-lock-type-face
  "Highlight types names.")

(defvar mjs-font-lock-keywords nil
  "Additional expressions to highlight in Metascript mode.")

(setq mjs-font-lock-keywords

      ;; Keywords
      `(,(rx symbol-start
             (or "new" "delete" "->")
             symbol-end)

        (,(rx symbol-start (or "var" "const" "fun")
              symbol-end) . mjs-def-face)

        (,(rx symbol-start (or "#meta" "#keep-meta")
              symbol-end) . mjs-modifier-face)

        (,(rx symbol-start (or "arguments" "require" "typeof" "instanceof")
              symbol-end) . mjs-builtin-face)

        (,(rx symbol-start (or "Object" "Array" "String"
                               "Function" "Number" "Math" "parseInt"
                               "Boolean" "Date" "JSON" "Error")
              symbol-end) . mjs-type-name-face)

        (,(rx symbol-start (or "true" "false" "null" "this" "undefined")
              symbol-end) . mjs-constant-face)

        (,(rx symbol-start (or "try" "catch" "finally" "throw"
                               "loop" "next" "if" "else"
                               "return" "do")
              symbol-end) . mjs-control-flow-face)

        ;; functions
        (,(rx symbol-start (or "var" "const" "fun" (seq "#def" (1+ word)) "#keepmacro" "#metaimport" "#external") (1+ space) (group (seq (1+ (or word ?_ ?- ?>)) (? (any "!?")))))
         (1 font-lock-function-name-face))

        ;; numbers
        (,(rx symbol-start (or (1+ digit) (seq "0x" (1+ hex-digit)))
              symbol-end) . mjs-number-face)


        ;; preprocessor
        (,(rx symbol-start (seq ?\# (1+ (or word ?_ ?- ?>)))
              symbol-end) . font-lock-warning-face)))


(defvar mjs-mode-syntax-table nil
  "Syntax table for Metascript files.")

(setq mjs-mode-syntax-table
      (let ((table (make-syntax-table)))
        ;; Give punctuation syntax to ASCII that normally has symbol
        ;; syntax or has word syntax and isn't a letter.
        (let ((symbol (string-to-syntax "_"))
              (sst (standard-syntax-table)))
          (dotimes (i 128)
            (unless (= i ?_)
              (if (equal symbol (aref sst i))
                  (modify-syntax-entry i "." table)))))
        (modify-syntax-entry ?$ "_" table)
        (modify-syntax-entry ?\- "_" table)
        (modify-syntax-entry ?\> "_" table)
        (modify-syntax-entry ?% "." table)
        ;; exceptions
        (modify-syntax-entry ?\; "<" table)
        (modify-syntax-entry ?\n ">" table)
        (modify-syntax-entry ?' "\"" table)
        (modify-syntax-entry ?\\ "\\" table)
        (modify-syntax-entry ?` "'" table)
        (modify-syntax-entry ?\# "_" table)
        table))

(defun mjs-indent-line ()
  "Indent current line of mjs code.
If the previous non empty line ends in any of `({[>' indentation is increased,
otherwise it stays the same."
  (interactive)
  (let (indent)
    (save-excursion
      (beginning-of-line)
      (setq indent
            (if (and (re-search-backward (rx (not (any ?\n whitespace))))
                     (looking-at "[{(\[>]"))
                (+ (current-indentation) mjs-indent-offset)
              (current-indentation))))
    (indent-to indent)))

(defvar mjs-mode-map
  (let ((map (make-sparse-keymap "Metascript")))
    (define-key map (kbd "RET") 'newline-and-indent)
    map))

(define-derived-mode mjs-mode fundamental-mode "MJS"
  "A major mode for editing Metascript source files."
  :syntax-table mjs-mode-syntax-table
  :group 'mjs-mode
  (setq-local comment-start "; ")
  (setq-local font-lock-defaults '(mjs-font-lock-keywords))
  (setq-local indent-line-function 'mjs-indent-line))

(add-hook 'mjs-mode-hook
          (lambda ()
            (when (setq indent-tabs-mode mjs-indent-tabs-mode)
              (setq tab-width mjs-indent-offset))))

(add-hook 'mjs-mode-hook 'rainbow-delimiters-mode)


                                        ;flymake integration
(defun flymake-mjs-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "mjs" (list "check" local-file))))

(setq flymake-allowed-file-name-masks
      (cons '(".+\\.mjs$"
              flymake-mjs-init
              flymake-simple-cleanup
              flymake-get-real-file-name)
            flymake-allowed-file-name-masks))

(setq flymake-err-line-patterns ; regexp file-idx line-idx col-idx (optional) text-idx(optional), match-end to end of string is error text
      (cons '("\\([^(]*\\)(\\([0-9]+\\),\\([0-9]+\\))\: \\(.+\\)"
              1 2 3 4)
            flymake-err-line-patterns))

(defcustom mjs-implies-flymake t
  "if mjs-mode should imply `flymake-mode', default is t. "
  :type 'boolean
  :group 'mjs-mode)

(add-hook 'mjs-mode-hook
          (lambda ()
            (when mjs-implies-flymake
              (flymake-mode))))

; custom symbol display

(defcustom mjs-display-custom-symbols t
  "if mjs-mode should replace certain programming language symbols such
 as `fun' and `#->' by nicer looking one such as `ƒ' and `𝝺', default is t."
  :type 'boolean
  :group 'mjs-mode)

(add-hook 'mjs-mode-hook
          (lambda ()
            (when mjs-display-custom-symbols
              (font-lock-add-keywords
               nil
               `(("\\_<\\(#->\\)\\_>"
                  (0 (progn (compose-region (match-beginning 1)
                                            (match-end 1)
                                            "λ")
                            'mjs-def-face)))

                 ("\\_<\\(fun\\)\\_>"
                  (0 (progn (compose-region (match-beginning 1)
                                            (match-end 1)
                                            "ƒ")
                            'mjs-def-face)))

                 ("\\_<\\(->\\)\\_>"
                  (0 (progn (compose-region (match-beginning 1)
                                            (match-end 1)
                                            "→")
                            'mjs-def-face)))

                 ("\s\\(!=\\)\s"
                  (0 (progn (compose-region (match-beginning 1)
                                            (match-end 1)
                                            "≠")
                            nil)))

                 ("\s\\(>=\\)\s"
                  (0 (progn (compose-region (match-beginning 1)
                                            (match-end 1)
                                            "≥")
                            nil)))

                 ("\s\\(<=\\)\s"
                  (0 (progn (compose-region (match-beginning 1)
                                            (match-end 1)
                                            "≤")
                            nil)))

                 ("\s\\(==\\)\s"
                  (0 (progn (compose-region (match-beginning 1)
                                            (match-end 1)
                                            "≣")
                            nil)))

                 ("\\_<\\(#it\\)\\_>"
                  (0 (progn (compose-region (match-beginning 1)
                                            (match-end 1)
                                            "ϰ")
                            'mjs-builtin-face))))))))
(provide 'mjs-mode)
