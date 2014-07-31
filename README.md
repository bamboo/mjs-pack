## Metascript Live Pack

A hackable [Metascript](https://github.com/massimiliano-mantione/metascript) language mode packaged for [emacs-live](http://overtone.github.io/emacs-live).

![screenshot](mjs-mode.png)

## Features

1. syntax highlighting with support for nice rendering of lambdas and other constructs
2. flymake integration for checking compilation errors as you type
3. simple auto indentation


## Installation

git clone into your ~/.live-packs directory and add
```lisp
(live-add-packs '(~/.live-packs/mjs-pack))
```
to your ~/.emacs-live.el

## Configuration

M-x customize-group mjs-mode

 Have fun!
