[![MELPA](https://melpa.org/packages/fstar-mode-badge.svg)](https://melpa.org/#/fstar-mode) [![Build Status](https://travis-ci.org/FStarLang/fstar-mode.el.svg?branch=master)](https://travis-ci.org/FStarLang/fstar-mode.el)

## Use F* in Emacs!

![Screenshot](img/fstar-mode.png)

## Setup

F*-mode requires Emacs 24.3 or newer, and is distributed through [MELPA](https://melpa.org).

1.  Add the following to your init file (usually `.emacs`) if it is not already there:

    ```elisp
    (require 'package)
    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
    (package-initialize)
    ```

2.  Restart Emacs, then run <kbd>M-x package-refresh-contents</kbd> and <kbd>M-x package-install RET fstar-mode RET</kbd>. Future updates can be downloaded using <kbd>M-x list-packages U x y</kbd>.

3.  If `fstar.exe` and `z3` are not already in your path, set the `fstar-executable` and `z3-executable` variables:

    ```elisp
    (setq-default fstar-executable "PATH-TO-FSTAR.EXE")
    (setq-default fstar-smt-executable "PATH-TO-Z3(.EXE)")
    ```

`fstar-mode` is compatible with Tramp: if you open an F* file on a remote machine, `fstar-mode` run F* remotely [over SSH](#editing-remote-f*-files).

## Keybindings

:zap: indicates keybindings available once F* is running.<br/>
:sparkles: indicates features added since the latest F* release.<br/>
:unicorn: indicates features that require yet-unmerged patches to F*.<br/>

### General

Key                 | Action
--------------------|----------------------------------
`C-h .`             | Show current error in echo area
`C-h M-w`           | Copy current error message
`TAB`               | Indent
`S-TAB`             | Unindent
`C-RET`             | :zap: :sparkles: Autocomplete word at point
`M-.`               | :zap: :sparkles: Jump to definition
`C-c C-d`           | :zap: :sparkles: Show documentation of symbol at point (repeat to hide docs)
`<menu>`, `M-<f12>` | :zap: :sparkles: Show type and docs in a small inline window (repeat to hide the window)

### Matching

Key                 | Action
--------------------|----------------------------------
`C-c C-s C-c`       | :zap: :unicorn: Case split: insert a match, or refine match at point

### Interactive mode

Proof-General            | :atom: Atom   | Action
-------------------------|---------------|----------------------------------------------------------
`C-c C-n`                | `C-S-n`       | :zap: Send the next paragraph (terminated by two empty lines) to F*
`C-c C-u` or `C-c C-p`   | `C-S-p`       | :zap: Retract the last paragraph
`C-c RET` or `C-c C-RET` | `C-S-i`       | :zap: Send everything up to the current point to F*
`C-c C-l`                | `C-S-l`       | :zap: Send everything up to the current point to F*, in lax mode
`C-c C-x`                | `C-M-c`       | :zap: Kill the F* subprocess
`C-c C-c`                | `C-M-S-c`     | :zap: Send an interrupt signal to Z3.  This (generally) interrupts the currently running verification task.

Use <kbd>M-x customize-variable RET fstar-interactive-keybinding-style RET</kbd> to pick a keybinding style. The default is Proof-General; the other option is Atom. Please be aware of the [current restrictions](https://github.com/FStarLang/FStar/wiki/Dealing-with-F%E2%98%85-dependencies#when-invoking-f-in-interactive-mode) on the interactive-mode.

### Completion

These keybindings are available during completion only:

Key     | Action
--------|------------------------------------------------
`C-w`   | Show definition of current completion candidate
`C-s`   | Search among completion candidates

## Advanced topics and customization

### Enabling and disabling individual F* mode components

Use <kbd>M-x customize-variable RET fstar-enabled-modules RET</kbd> to choose which parts of fstar-mode to enable.

### Real-time verification

:unicorn: Customize the variable `fstar-flycheck-checker` to pick your favorite style of real-time verification (full-buffer verification or light, fast syntax checks in addition to proof-general style verification).  F*-mode's real-time checking uses `flycheck-mode` under the hood: try `M-x customize-group flycheck` to tweak Flycheck further.

### Completion

F*-mode's completion uses `company-mode` under the hood.  Try `M-x customize-group company`, and in particular consider changing `company-idle-delay` to get completions faster.

### Using F*-mode for `.fsi` files

Use the following snippet:

```elisp
(add-to-list 'auto-mode-alist '("\\.fsi\\'" . fstar-mode))
```

### Including non-standard libraries when using fstar-mode

Add the following line to your `.emacs`.

```elisp
(setq fstar-subp-prover-args '("--include" "<your-path>"))
```

Use `C-h v fstar-subp-prover-args` for more details.

### Editing remote F* files

F*-mode is compatible with Emacs' Tramp.  To use F*-mode over tramp:

* Set `fstar-executable` and `z3-executable` appropriately (if F* and Z3 are in your path on the remote machine it's enough to use `fstar.exe` and `z3`; otherwise, use their full paths on the remote machine — e.g. `~/FStar/bin/fstar.exe`),
* Open a remote file (`C-x C-f /sshx:username@remote-server: RET test.fst RET`).  Processing this file should invoke a remote F* through SSH.

## Troubleshooting

### Missing characters

Boxes instead of math symbols are most likely due to missing fonts. DejaVu Sans Mono, [Symbola](http://shapecatcher.com/downloads/Symbola706.zip), FreeMono, STIX, Unifont, Segoe UI Symbol, Arial Unicode and Cambria Math are all good candidates. If Emacs doesn't pick up on the new fonts after a restart, the following snippet (add it to your .emacs) should help:

```elisp
(set-fontset-font t 'unicode (font-spec :name "YOUR USUAL EMACS FONT") nil 'append)
(set-fontset-font t 'unicode (font-spec :name "SOME FONT WITH GOOD COVERAGE AS LISTED ABOVE") nil 'append)
```

### Fonts for specific characters

Use the following snippet to use `Symbola` for `∀` only:

```elisp
(set-fontset-font t (cons ?∀ ?∀) "Symbola" nil 'prepend)
```

### Cygwin

If you're using Emacs on Windows with Cygwin (😱), you can install [windows-path.el](https://www.emacswiki.org/emacs/windows-path.el)

The short version is to do:

```
$ wget https://www.emacswiki.org/emacs/download/windows-path.el
```

Then, in your .emacs
```elisp
(require 'windows-path "<path to windows-path.el>")
(windows-path-activate)
```

### Building from source

Setup MELPA as described above, then install the dependencies and clone the repo to a directory of your choice (`~/.emacs.d/lisp/fstar-mode.el` for example). Optionally byte-compile `fstar-mode.el`, and finally add the following to your init file:

```elisp
(require 'fstar-mode "~/.emacs.d/lisp/fstar-mode.el/fstar-mode.el")
```
