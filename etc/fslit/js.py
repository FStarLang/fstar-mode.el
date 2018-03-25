#!/usr/bin/env python2
# -*- coding: utf-8 -*-

import os
import docutils
from sphinx.config import Config
from sphinx.errors import ExtensionError, ConfigError
from sphinx.application import Sphinx

from . import docutils4fstar

MISSING_FSTARJS_MESSAGE = "Directory '{}/fstar.js/' not found. Please \
download an fstar.js build from GitHub and decompress it into your \
'_static' folder."

def ensure_fstar_js(static_path):
    """Ensure that fstar.js was downloaded."""
    for pth in static_path:
        candidate = os.path.join(pth, 'fstar.js')
        if os.path.isdir(candidate):
            return candidate
    raise ExtensionError(MISSING_FSTARJS_MESSAGE)

def setup_js_assets(app): # type: Sphinx -> ()
    if app.builder.name == "html":
        fstar_js_path = ensure_fstar_js(app.config.html_static_path)

        # Adding to extra_path ensures that the files are copied to the
        # output but ignored when looking for sources.
        app.config.html_extra_path.append(fstar_js_path)

        # This configures FStar.{IDE,CLI}.WORKER_DIRECTORY, sets the file name,
        # and loads the literate client.
        app.add_javascript("fslit.fstarjs-config.js")

        # Listed here are only the client scripts (not the worker ones), as
        # these are the only ones that need to be directly included in the HTML.
        app.add_stylesheet("fstar.js/fstar.ide.css")
        app.add_stylesheet("fstar.js/fstar.cli.css")
        app.add_javascript("fstar.js/fstar.global-object.js")
        app.add_javascript("fstar.js/fstar.ide.utils.js")
        app.add_javascript("fstar.js/fstar.client.utils.js")
        app.add_javascript("fstar.js/fstar.ide.protocol.js")
        app.add_javascript("fstar.js/fstar.ide.client.js")
        app.add_javascript("fstar.js/fstar.ide.literate.client.js")
        app.add_javascript("fstar.js/fstar.cli.protocol.js")
        app.add_javascript("fstar.js/fstar.cli.client.js")
        # (Sphinx adds a static “_static/” prefix to all relative paths)

        # These two are used for displaying goals
        app.add_javascript("https://cdnjs.cloudflare.com/ajax/libs/mustache.js/2.3.0/mustache.js")
        app.add_javascript("https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.36.0/addon/runmode/runmode.min.js")

FSTAR_JS_CODA = """\
<!-- F*.js configuration -->
<script>
__FSTAR_JS_CURRENT_FST_FILE_NAME__ = "{{{FILENAME}}}"
</script>
<!-- End of F*.js configuration -->
"""

def insert_fstarjs_script_tags(_app, doctree, fromdocname):
    js = FSTAR_JS_CODA.replace("{{{FILENAME}}}", fromdocname + ".fst")
    doctree.append(docutils.nodes.raw("", js, format="html"))

def setup(app):
    app.connect('builder-inited', setup_js_assets)
    app.connect('doctree-resolved', insert_fstarjs_script_tags)

    return {'version': '0.1', "parallel_read_safe": True}
