# -*- coding: utf-8 -*-

from docutils import nodes
from sphinx.domains import Domain
from sphinx.util.osutil import relative_uri
from . import docutils4fstar

# FIXME Should use copy_asset_file to copy the fslit CSS

# Export here so config files can refer to just this module
LiterateFStarParser = docutils4fstar.LiterateFStarParser

class FStarDomain(Domain):
    """A domain to document F* code.

    Sphinx has a notion of “domains”, used to tailor it to a specific language.
    Domains mostly consist in descriptions of the objects that we wish to
    describe, as well as domain-specific roles and directives.

    Each domain is responsible for tracking its objects, and resolving
    references to them.
    """

    name = 'fstar'
    label = 'F*'

    object_types = dict() # ObjType (= directive type) → (Local name, *xref-roles)

    directives = dict() # Directive → Object

    roles = { # FIXME
        'type': docutils4fstar.FStarTypeRole
    }

    indices = []

    data_version = 1
    initial_data = {
        # Collect everything under a key that we control, since Sphinx adds
        # others, such as “version”
        'objects' : {}
    }


def unfold_folded_fst_blocks(app, doctree, _fromdocname):
    if app.buildername == 'html':
        for node in doctree.traverse(docutils4fstar.fst_node):
            node.replace_self(node.original_node)

def process_external_editor_references(app, doctree, fromdocname):
    """Adjust references to the external editor."""
    if app.buildername == "html":
        for node in doctree.traverse(docutils4fstar.standalone_editor_reference_node):
            node['refuri'] = relative_uri(app.builder.get_target_uri(fromdocname), node['docpath'])
    else:
        node.parent.remove(node)

def setup(app):
    """Register the F* domain"""

    app.add_domain(FStarDomain)

    for role in docutils4fstar.ROLES:
        app.add_role(role.role, role)

    for node in docutils4fstar.NODES:
        app.add_node(node,
                     html=(node.visit, node.depart),
                     latex=(node.visit, node.depart),
                     text=(node.visit, node.depart))

    for directive in docutils4fstar.DIRECTIVES:
        getattr(directive, "setup", lambda _: None)(app.srcdir)
        app.add_directive(directive.directive, directive)

    for transform in docutils4fstar.TRANSFORMS:
        app.add_transform(transform)

    app.connect('doctree-resolved', unfold_folded_fst_blocks)
    app.connect('doctree-resolved', process_external_editor_references)

    # FIXME
    # Add fslit.css ("copy asset"?)
    # app.add_stylesheet("….css")

    return {'version': '0.1', "parallel_read_safe": True}
