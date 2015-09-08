# `fstar-mode`

## Use F* in Emacs!

Includes:

* (Moderately complex) syntax highlighting
* Basic (control-points-based) indentation
* Prettification of mathematical symbols using `prettify-symbols-mode`
* Real-time verification using `flycheck`
* Experimental support for) interactive (Coq-style) verification [GH-3]

![Screenshot](img/fstar-mode.png)

# Setup

Copy the following into an empty buffer, and run <kbd>M-x eval-buffer</kbd>.

```elisp
(let ((url "https://raw.githubusercontent.com/FStarLang/fstar.el/master/fstar-mode.el"))
  (with-current-buffer (url-retrieve-synchronously url)
    (package-install-from-buffer)))
```

Alternatively, use <kbd>M-:</kbd> to open the `eval-expression` mini-buffer, and paste the snippet above.

If `fstar` is not already in your path, set the `flycheck-fstar-executable` variable:

```elisp
(set-default flycheck-fstar-executable "PATH-TO-FSTAR.EXE")
```

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
