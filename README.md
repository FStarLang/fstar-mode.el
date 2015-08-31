# `fstar-mode`

## Use F* in Emacs!

Includes:

* (Moderately complex) syntax highlighting
* Prettification of mathematical symbols using `prettify-symbols-moe`
* Real-time verification using `flycheck`

Indentation support is still extremely rudimentary.

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
