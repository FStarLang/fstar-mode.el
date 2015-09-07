# `fstar-mode`

## Use F* in Emacs!

Includes:

* (Moderately complex) syntax highlighting
* Basic (control-points-based) indentation
* Prettification of mathematical symbols using `prettify-symbols-mode`
* Real-time verification using `flycheck`
* Experimental support for) interactive (Coq-style) verification [GH-3]

![Screenshot](img/fstar-mode.png)

## Setup

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

## Manual setup

Clone the repo to a directory of your choice `dir`. Add the following to your `.emacs`:

```elisp
(require 'fstar-mode "dir/fstar-mode.el")
```

Make sure to install the dependencies: <kbd>M-x package-install RET flycheck RET</kbd> should do it.

## Customization

Use <kbd>M-x customize-variable RET fstar-enabled-modules RET</kbd> to choose which pars of fstar-mode to enable.

## Fonts

DejaVu Sans Mono, Symbola, FreeMono, STIX, Unifont, Segoe UI Symbol, Arial Unicode and Cambria Math are all good candidates. If Emacs doesn't pick up on the new fonts after a restart, the following snippet (add it to your .emacs) should help:

(set-fontset-font t 'unicode (font-spec :name "YOUR USUAL EMACS FONT") nil 'append)
(set-fontset-font t 'unicode (font-spec :name "SOME FONT WITH GOOD COVERAGE AS LISTED ABOVE") nil 'append)
