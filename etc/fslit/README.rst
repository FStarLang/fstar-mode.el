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

In ``fstar-mode``, pressing `C-c C-S-a` toggles between literate F\* sources and
the corresponding reST document.  Consult the documentation of ``rst-mode``
(`C-h f rst-mode`) to learn about editing reST files in Emacs, and the `quick
start guide <http://www.sphinx-doc.org/en/stable/rest.html>`_ for general
information about reST.

Compiling literate F\* files
============================

Docutils
--------

Use the ``fslit/fst2html.py`` script to render a literate F* file as a standalone HTML file.  This works well for quick experiments::

   $ fslit/fst2html.py example.fst > example.html

Sphinx
------

For larger examples, use Sphinx (they have a good `tutorial
<http://www.sphinx-doc.org/en/stable/tutorial.html>`_). Here is how to create a
new Sphinx project::

   $ mkdir fstar-book; cd fstar-book/

   $ git clone --depth 1 https://github.com/FStarLang/fstar-mode.el
   … Checking connectivity... done.

   $ sphinx-quickstart
   …
   > Root path for the documentation [.]: .
   > Separate source and build directories (y/n) [n]: n
   > Name prefix for templates and static dir [_]: _
   > Project name: …
   > Author name(s): …
   > Project version []: …
   > Project release […]: …
   > Project language [en]: …
   > Source file suffix [.rst]: .fst
   > Name of your master document (without suffix) [index]: index
   …
   > mathjax: include math, rendered in the browser by MathJax (y/n) [n]: y
   …
   > Create Makefile? (y/n) [y]: y
   … Finished: An initial directory structure has been created.

Open the generated ``conf.py`` file, and make the following changes:

- Insert the following after the comment saying *If extensions […] are in another directory, add these directories to sys.path here.*::

     import os
     import sys
     sys.path.insert(0, os.path.abspath('fstar-mode.el/etc/'))

- Add ``'fslit.sphinx4fstar',`` to the ``extensions`` line::

     extensions = [
         'sphinx.ext.todo',
         'sphinx.ext.mathjax',
         'fslit.sphinx4fstar',    ← here
     ]

- Adjust the source_suffix line as follows::

     source_suffix = ['.rst', '.fst']

- Discard the generated ``index.fst``, and replace it with your own literate F\*
  document.

Use ``make html`` to confirm that everything is working.  Your website is in
``_build/html/index.html``.

Literate F\* roles and directives
=================================

FIXME

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
