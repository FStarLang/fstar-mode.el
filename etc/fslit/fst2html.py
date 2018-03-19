#!/usr/bin/env python2
# -*- coding: utf-8 -*-

"""Compile a single literate F* document to HTML."""

from __future__ import unicode_literals

import locale
from docutils.core import publish_cmdline, default_description

if __name__ == "__main__" and __package__ is None:
    from os import sys, path
    sys.path.append(path.dirname(path.dirname(path.abspath(__file__))))

from fslit import docutils4fstar

try:
    locale.setlocale(locale.LC_ALL, '')
except locale.Error:
    pass

from docutils.writers.html5_polyglot import HTMLTranslator

docutils4fstar.register()
docutils4fstar.add_nodes(HTMLTranslator)
publish_cmdline(writer_name='html5',
                parser=docutils4fstar.LiterateFStarParser(),
                settings_overrides={'syntax_highlight': 'none'},
                description='Build an HTML5 document from a literate F* file.' + default_description)
