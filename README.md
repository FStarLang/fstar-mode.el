# `fstar-mode`

## Use F* in Emacs!

Includes:

* (Moderately complex) syntax highlighting
* Basic (control-points-based) indentation
* Prettification of mathematical symbols using `prettify-symbols-mode`
* Real-time verification (requires `flycheck`, see `fstar-enabled-modules`)
* (Experimental support for) interactive verification (<i>à la</i> Proof-General)

![Screenshot](img/fstar-mode.png)

# Setup

F*-mode requires Emacs 24.3 or newer, and its dependencies are hosted on [MELPA](https://melpa.org). Add the following to your init file (usually `.emacs`):

```elisp
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
```

Then copy the following into an empty buffer, and run <kbd>M-x eval-buffer</kbd>.

```elisp
(let ((url "https://raw.githubusercontent.com/FStarLang/fstar.el/master/fstar-mode.el"))
  (with-current-buffer (url-retrieve-synchronously url)
    (package-install-from-buffer)))
```

Finally, if `fstar.exe` is not already in your path, set the `fstar-executable` variable:

```elisp
(set-default fstar-executable "PATH-TO-FSTAR.EXE")
```

Restart Emacs, and you should be good to go.

## Customization

### Use Atom keybindings

Use <kbd>M-x customize-variable RET fstar-interactive-keybinding-style RET</kbd> to pick a keybinding style. The default is Proof-General; the other option is Atom.

```
Emacs                 Atom     Action
-------------------------------------------------------------------------------------
C-c C-n               C-S-n    Process the next block (terminated by two empty lines)
C-c C-u or C-c C-p    C-S-p    Retract last block
C-c RET or C-c C-RET  C-S-i    Process the file up to the current point
C-c C-x               C-M-c    Kill the F* process
```

### Disable individual F* mode components

Use <kbd>M-x customize-variable RET fstar-enabled-modules RET</kbd> to choose which parts of fstar-mode to enable.

## Fixing font issues

DejaVu Sans Mono, Symbola, FreeMono, STIX, Unifont, Segoe UI Symbol, Arial Unicode and Cambria Math are all good candidates. If Emacs doesn't pick up on the new fonts after a restart, the following snippet (add it to your .emacs) should help:

```elisp
(set-fontset-font t 'unicode (font-spec :name "YOUR USUAL EMACS FONT") nil 'append)
(set-fontset-font t 'unicode (font-spec :name "SOME FONT WITH GOOD COVERAGE AS LISTED ABOVE") nil 'append)
```

## Setting up

Clone the repo to a directory of your choice `dir`. Add the following to your `.emacs`:

```elisp
(require 'fstar-mode "dir/fstar-mode.el")
```

Make sure to install the dependencies: <kbd>M-x package-install RET flycheck RET</kbd> should do it.
