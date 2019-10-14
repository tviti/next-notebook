
# Table of Contents

1.  [Getting started](#org449baee)
2.  [Features](#orgfecb50b)
    1.  [Context aware cell editing in Emacs](#org6112e41)
    2.  [Keyboard based notebook interactions](#org588fb10)
        1.  [Keybindings](#orgb04397e)

A *next browser* mode for editing Jupyter Notebooks.

**NOTE:** This is largely a **WIP**, with features added on the fly as I find myself
 wanting them during my day to day work.

For more information on `next-browser`, see:

-   <https://github.com/next-browser/next>
-   <https://github.com/atlas-engineer/next/blob/master/documents/MANUAL.org>


<a id="org449baee"></a>

# Getting started

I'm not sure *if* there's a prescribed mechanism for adding
user packages to `next`, but I think you should be able to just `(load
  "/path/to/jupyter-nb-mode.lisp")` somewhere in your `init.lisp`. Then, navigate to the
URL of a running notebook server in `next`, open a `.ipynb` file, and then
call `M-x jupyter-nb-mode` from the notebook's buffer.


<a id="orgfecb50b"></a>

# Features


<a id="org6112e41"></a>

## Context aware cell editing in Emacs

The command `edit-cell` will open the selected cell's contents in a new Emacs
buffer. The cell contents are transfered between Next and Emacs by means of a
temporary file, whose extension is chosen based on the selected cell's
type. `code` cells are assigned `.py` extensions, `markdown` cells `.md`, and
`raw` cells `.txt`. Thus, when the cell's contents are opened in Emacs, the
associated buffer should already be setup with the correct mode(s) for
editing that sort of text.

Similarly, you can edit the selected cell's metadata with the command
`edit-cell-metadata`, which will spawn a `.JSON` temp file.

Editing environments based on a package like `python-mode` or `elpy` will be
completely unaware of what's going on in the notebook kernel's namespace, so
the linter will probably complain about references that weren't imported or
defined in the active cell. One possible solution to this that I'd like to
try (and which I think could potentially be really slick), is to setup
[jupyter-mode](https://github.com/dzop/emacs-jupyter) as the `.py` editing environment in Emacs, and then configure it
to hook into the active notebook's kernel.


<a id="org588fb10"></a>

## Keyboard based notebook interactions

There's a lot of work to be done on this front, but at the moment you can bind
the commands `select-next-cell` and `select-prev-cell`. Execution can be done
through `execute-selected-cells` and `execute-all-cells`. There are a bunch
of other commands for navigation, cell-insertion, copying, pasting, saving,
etc&#x2026; I basically add these as I find myself wishing they existed.


<a id="orgb04397e"></a>

### Keybindings

As an `Evil` user whose never spent a significant amount of time around the
vanilla Emacs keybindings, I've really only defined a `vi-map` for this
(there's an `emacs-map` but it's pretty sparse). If anybody out there is
actually using this, suggestions for sensible non-`evil` Emacs users are
appreciated.

**NOTE:** In the `vi-map`, I have bound `o` and `O` to insert a cell below/above
the currently selected cell (resp.), which **overrides the default `root-mode`
bindings for `set-url-current-buffer` and `set-url-new-buffer`**.

