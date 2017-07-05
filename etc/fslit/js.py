#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from . import docutils4fstar

def setup(app):
    # Sphinx adds a static “_static/” prefix to all relative paths

    app.add_stylesheet("fstar.js/fstar.ide.css")
    app.add_stylesheet("fstar.js/fstar.cli.css")
    app.add_stylesheet("fstar.js/cm.tango.css")

    app.add_stylesheet("https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.27.2/codemirror.min.css")
    app.add_javascript("https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.27.2/codemirror.min.js")
    app.add_javascript("https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.27.2/mode/mllike/mllike.min.js")

    app.add_javascript("fstar.js/fstar.global-object.js")
    app.add_javascript("fstar.js/fstar.ide.utils.js")
    app.add_javascript("fstar.js/fstar.cm.js")
    app.add_javascript("fstar.js/fstar.client.utils.js")
    app.add_javascript("fstar.js/fstar.ide.protocol.js")
    app.add_javascript("fstar.js/fstar.ide.client.js")
    app.add_javascript("fstar.js/fstar.ide.literate.client.js")
    app.add_javascript("fstar.js/fstar.cli.protocol.js")
    app.add_javascript("fstar.js/fstar.cli.client.js")
    app.add_javascript("fstar.sphinx.js")

    app.connect('doctree-resolved', docutils4fstar.insert_fstarjs_script_tags)
    return {'version': '0.1'}
