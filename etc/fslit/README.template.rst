====================================================================
 `fslit`: Utilities for writing and rendering literate F\* programs
====================================================================

Setting up
==========

Install ``docutils`` and ``sphinx``::

   pip install --user docutils sphinx

… then clone this repository.

Writing literate F\* files
==========================

Literate F\* files are interleavings of F\* code and lines prefixed with ``///``
(called literate comments).  For example::

   /// Let's start with a simple program:    ← This is a literate comment

   (** Compute the average of [nums]. **)
   let avg nums =
     sum nums / List.length nums

Literate comments are written in reStructuredText (reST): the example above maps
to the following reST document::

   Let's start with a simple program:

   .. fst::

      (** Compute the average of [nums]. **)
      let avg nums =
        sum nums / List.length nums

Consult the `reStructuredText quick start guide
<https://www.sphinx-doc.org/en/stable/rest.html>`_ for general information about
reST, and the following sections for details on F\*-specific directives.

Emacs support
-------------

``fstar-mode`` displays literate comment markers as a solid line in the margin
(┃), and highlights reST syntax errors in literate comments as you type (using
``fslit``\ 's ``lint.py`` script).

Additionally, pressing `C-c C-S-a` in ``fstar-mode`` toggles between literate
F\* sources and a reST version of the same document, suitable for large reST
edits (read the documentation of ``rst-mode`` with `C-h f rst-mode` to learn
about editing reST files in Emacs).

Compiling literate F\* files
============================

Docutils
--------

Use the ``fslit/fst2html.py`` script to render a literate F* file as a standalone HTML file.  This works well for quick experiments::

   $ fslit/fst2html.py example.fst > example.html

From Emacs, simply run `M-x fstar-literate-preview`.

Sphinx
------

For larger examples, use Sphinx (they have a good `tutorial
<http://www.sphinx-doc.org/en/stable/tutorial.html>`_). Here is how to create a
new Sphinx project::

   $ mkdir fstar-book; cd fstar-book/

   $ git clone --depth 1 https://github.com/FStarLang/fstar-mode.el
   … Checking connectivity... done.

   $ sphinx-quickstart --ext-mathjax --extensions fslit.sphinx4fstar \
       --suffix .fst --quiet --author '<you>' --project '<proj-name>'

Open the generated ``conf.py`` file, and make the following changes:

- Insert the following after the comment saying *If extensions […] are in another directory, add these directories to sys.path here.*::

     import os
     import sys
     sys.path.insert(0, os.path.abspath('fstar-mode.el/etc/'))

- Adjust the ``source_suffix`` line as follows::

     source_suffix = ['.rst', '.fst']

- Discard the generated ``index.fst``, and replace it with your own literate F\*
  document.

Use ``make html`` to confirm that everything is working.  Your website is in
``_build/html/index.html``.

Literate F\* roles and directives
=================================

Directives
----------

[DIRECTIVES]

Roles
-----

[ROLES]

Literate F\* syntax notes
=========================

By default, code blocks are placed at the same indentation level as the last
preceding text::

   /// .. note::
   ///
   ///    The following code is captured in the note:

   let a = 1

   ↓

   .. note::

      The following code is captured in the note:

      .. fst::

         let a = 1

You can avoid this using an explicit ``.. fst::`` marker::

   /// .. note::
   ///
   ///    The following code is not captured in the note.
   ///
   /// .. fst::

   let a = 1

   ↓

   .. note::

      The following code is not captured in the note.

   .. fst::

      let a = 1
