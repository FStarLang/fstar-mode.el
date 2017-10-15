#!/usr/bin/env python2
# -*- coding: utf-8 -*-

import os
from sphinx.errors import ExtensionError

from . import docutils4fstar

# FIXME
# def download_fstar_js():
#     URL =
#     with urllib.request.urlopen(url) as response:
#         with gzip.GzipFile(fileobj=response) as uncompressed:
#             file_header = uncompressed.read(64) # a `bytes` object
#     urllib.urlretrieve()

def ensure_fstar_js(static_path):
    """Ensure that fstar.js was downloaded."""
    for pth in static_path:
        candidate = os.path.join(pth, 'fstar.js')
        if os.path.isdir(candidate):
            return candidate
    error_message = "Please download an fstar.js build from GitHub" \
        "and decompress it into your '_static' folder."
    raise ExtensionError(error_message)

def setup_js_assets(app):
    ensure_fstar_js(app.html_static_path)

    app.config.html_static_path.append(app.config.fstar_js_path)
    app.add_javascript("fslit.fstarjs-config.js")

    # Listed here are only the client scripts (not the worker ones), as
    # these are the only ones that need to be directly included in the HTML
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

def setup(app):
    # Sphinx adds a static “_static/” prefix to all relative paths
    if app.buildername == "html":
        app.connect('builder-inited', setup_js_assets)
        app.connect('doctree-resolved', docutils4fstar.insert_fstarjs_script_tags)
    return {'version': '0.1', "parallel_read_safe": True}
