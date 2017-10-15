#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# pylint: disable=too-few-public-methods,too-many-ancestors

import re
import os
import codecs
import itertools
import fnmatch

import docutils
from docutils import nodes, DataError
from docutils.transforms import Transform
from docutils.parsers.rst import Directive, directives, roles
from docutils.parsers.rst.directives import admonitions

from .translate import fst2rst_linums

# Constants
# =========

SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
ASSETS_PATH = os.path.join(SCRIPT_PATH, "assets")

# Utilities
# =========

INDENTATION_RE = re.compile("^ *")
def measure_indentation(line):
    return INDENTATION_RE.match(line).end()

def recompute_contents(directive, real_indentation):
    block_lines = directive.block_text.splitlines()
    block_header_len = directive.content_offset - directive.lineno + 1
    block_indentation = measure_indentation(directive.block_text)
    code_indentation = block_indentation + real_indentation
    lines = [ln[code_indentation:] for ln in block_lines[block_header_len:]]
    return lines

def parents(node):
    current = node.parent
    while current is not None:
        yield current
        current = current.parent

def find_parent(node, parent_class):
    return next((parent for parent in parents(node) if isinstance(parent, parent_class)), None)

def assert_attached_to(node, parent_class):
    if find_parent(node, parent_class) is None:
        raise DataError("'{}' blocks must appear in '{}' blocks."
                        .format(node.__class__.__name__, parent_class.__name__))

def set_source_info(directive, node):
    node.source, node.line = directive.state_machine.get_source_and_line(directive.lineno)

def mkdir(absdir):
    try:
        os.makedirs(absdir)
    except OSError:
        if not os.path.isdir(absdir):
            raise

def skip_blanks(iterator):
    while True:
        line = next(iterator)
        if line != "":
            return line

def join_blocks(blocks):
    for block in blocks:
        yield ""
        for line in block:
            yield line

# Directives
# ==========

# .. fixme
# --------

class FixmeAuthorsDirective(Directive):
    directive = "fixme-authors"

    has_content = True
    required_arguments = 0
    final_argument_whitespace = False
    option_spec = {}

    def run(self):
        env = self.state.document.settings.env
        if not hasattr(env, "fixme_authors"):
            env.fixme_authors = {}
        for line in self.content:
            nick, name = line.split(maxsplit=1)
            env.fixme_authors[nick] = name
        return []

class FixmeDirective(admonitions.BaseAdmonition):
    directive = "fixme"
    node_class = nodes.admonition

    has_content = True
    required_arguments = 1
    optional_arguments = 0
    final_argument_whitespace = False
    option_spec = {}

    def run(self):
        env = self.state.document.settings.env
        nicknames = getattr(env, "fixme_authors", {})
        author = nicknames.get(self.arguments[0], self.arguments[0])
        self.options["class"] = ["admonition-fixme"]
        self.arguments[0] = "FIXME ({})".format(author)
        return super(FixmeDirective, self).run()

# .. exercise, .. solution
# ------------------------

class standalone_editor_reference_node(nodes.reference):
    @staticmethod
    def visit(visitor, node):
        return visitor.visit_reference(node)
    @staticmethod
    def depart(visitor, node):
        return visitor.depart_reference(node)

class Artifact(object):
    PREAMBLE_END_MARKER = [["/" * 60]]
    MODNAME_FORBIDDEN_RE = re.compile("[^A-Za-z0-9_.]")
    MODULE_LINE_RE = re.compile("^ *module [A-Za-z0-9_.]+ *$")

    def __init__(self, docname, name, subdir, preamble, own_code):
        self.modname = Artifact.clean_modname(docname) + "." + name
        self.subdir, self.preamble, self.own_code = subdir, preamble, own_code

    @property
    def filename(self):
        return self.modname + ".fst"

    @staticmethod
    def clean_modname(name):
        modname = Artifact.MODNAME_FORBIDDEN_RE.sub("", name)
        if not modname:
            raise DataError("Can't make an F* module name from '{}'".format(name))
        return modname[0].upper() + modname[1:]

    @staticmethod
    def assemble_fstar_document(blocks, modname):
        yield "module {}".format(modname)
        module_line_found = False
        lines = join_blocks(blocks)
        try:
            while True:
                line = next(lines)
                if (not module_line_found) and Artifact.MODULE_LINE_RE.match(line):
                    module_line_found = True
                    line = skip_blanks(lines)
                yield line
        except StopIteration:
            pass

    def writeout(self, outdir):
        abspath = os.path.abspath(os.path.join(outdir, self.subdir, self.filename))
        with codecs.open(abspath, mode="w", encoding="utf-8") as out:
            sep = Artifact.PREAMBLE_END_MARKER
            blocks = itertools.chain(self.preamble, sep, self.own_code)
            for line in Artifact.assemble_fstar_document(blocks, self.modname):
                out.write(line)
                out.write("\n")

    def make_link(self):
        TEXT = "→ Open in standalone editor"
        CLASS = "fstar-standalone-editor-link"
        node = standalone_editor_reference_node('', TEXT, classes=[CLASS])
        node["docpath"] = os.path.join(self.subdir, self.filename)
        return nodes.paragraph('', '', node, classes=["fstar-standalone-editor-link-box"])

class TitledBlockBaseDirective(Directive):
    required_arguments = 0
    optional_arguments = 1
    final_argument_whitespace = True
    option_spec = { "class": directives.class_option,
                    "name": directives.unchanged }
    has_content = True

    label = None
    directive = None
    node_class = nodes.container

    def run(self):
        self.assert_has_content()

        mk_class = lambda name: self.directive + "-" + name
        contents = "\n".join(self.content)

        header_nodes = [nodes.inline(text=self.label, classes=[mk_class("label")])]
        if self.arguments:
            header_nodes.append(nodes.inline(text=self.arguments[0], classes=[mk_class("title")]))

        header_node = nodes.inline('', '', *header_nodes, classes=[mk_class("header")])
        body_node = nodes.container(contents, classes=[mk_class("body")])

        container_node = self.node_class(contents, header_node, body_node)
        container_node.header = header_node
        container_node["fname"] = self.options.get("save-as", None)
        container_node["classes"] = [self.directive] + self.options.get("class", [])
        self.add_name(container_node) # target
        set_source_info(self, container_node)

        self.state.nested_parse(self.content, self.content_offset, body_node)
        return [container_node]

# .. exercise
# ~~~~~~~~~~~

class exercise_node(nodes.container):
    subdir = "exercises"
    @staticmethod
    def visit(visitor, node):
        return visitor.visit_container(node)
    @staticmethod
    def depart(visitor, node):
        return visitor.depart_container(node)

class IncludeExcludeFilter(object):
    def __init__(self, include_re, exclude_re):
        self.include_re, self.exclude_re = include_re, exclude_re

    @staticmethod
    def parse(patterns_str):
        if patterns_str:
            regexes = (fnmatch.translate(pat) for pat in patterns_str.split())
            return re.compile(r"^(" + r"|".join(regexes) + r")$")

    @staticmethod
    def from_strings(includes, excludes):
        include_re = IncludeExcludeFilter.parse(includes)
        exclude_re = IncludeExcludeFilter.parse(excludes)
        return IncludeExcludeFilter(include_re, exclude_re)

    def match_tag(self, tag):
        return self.include_re.match(tag) and (not self.exclude_re.match(tag))

    def match_node(self, node):
        # print("Applying {} to {}".format((self.include_re, self.exclude_re), node))
        # ‘*’ filters need at least one tag, so use "?" as a default
        tags = node.get("tags") or ["?"]
        keep = True
        if self.include_re:
            keep = keep and any(self.include_re.match(t) for t in tags)
        if self.exclude_re:
            keep = keep and not any(self.exclude_re.match(t) for t in tags)
        return keep

    def apply(self, nodes_collection):
        return [n for n in nodes_collection if self.match_node(n)]

class ExerciseDirective(TitledBlockBaseDirective):
    option_spec = { "class": directives.class_option,
                    "name": directives.unchanged,
                    "save-as": directives.unchanged,
                    "include": directives.unchanged,
                    "exclude": directives.unchanged }

    label = "Exercise"
    directive = "exercise"
    node_class = exercise_node

    def run(self):
        [node] = super(ExerciseDirective, self).run()
        includes = self.options.get("include")
        excludes = self.options.get("exclude")
        node.filters = IncludeExcludeFilter.from_strings(includes, excludes)
        return [node]

# .. solution
# ~~~~~~~~~~~

class solution_node(nodes.container):
    subdir = "solutions"
    @staticmethod
    def visit(visitor, node):
        return visitor.visit_container(node)
    @staticmethod
    def depart(visitor, node):
        return visitor.depart_container(node)

class SolutionDirective(TitledBlockBaseDirective):
    label = "Solution"
    directive = "solution"
    node_class = solution_node

# F* listings
# -----------

class FStarListingBaseDirective(Directive):
    EXPECTED_INDENTATION = 3
    HIDDEN_BLOCK_RE = re.compile(re.escape("(* {{{")
                                 + "(?P<annotation>.*?)"
                                 + re.escape ("*)")
                                 + ".*?"
                                 + re.escape("(* }}} *)"), flags=re.DOTALL)
    option_spec = { "class": directives.class_option,
                    "name": directives.unchanged,
                    "tags": directives.unchanged }
    has_content = True
    node_class = nodes.literal_block

    @staticmethod
    def collapse_hidden_blocks(text):
        return FStarListingBaseDirective.HIDDEN_BLOCK_RE.sub(r"(* …\g<annotation>*)", text)

    def run(self):
        self.assert_has_content()

        roles.set_classes(self.options)
        classes = ["code", "fstar"] + self.options.get("classes", [])

        lines = recompute_contents(self, FStarBlockDirective.EXPECTED_INDENTATION)

        original_code = "\n".join(lines)
        # Sphinx highlights code when ``node.raw == node.astext()``. We don't
        # want highlighting here, so we use a dummy ``raw`` value
        original_node = self.node_class("<no-highlighting>", original_code, classes=classes)

        collapsed_code = self.collapse_hidden_blocks(original_code)
        collapsed_node = self.node_class(collapsed_code, collapsed_code, classes=classes)
        collapsed_node.original_node = original_node

        tags = self.options.get("tags", "").split()
        for node in [original_node, collapsed_node]:
            node["tags"] = tags
            node.lines = lines

            self.add_name(node)
            set_source_info(self, node)

        return [collapsed_node]

# .. tag-all
# ~~~~~~~~~~
class tag_all_node(nodes.literal_block):
    pass

class TagAllDirective(Directive):
    directive = "tag-all"
    option_spec = {}
    has_content = False
    optional_arguments = 1
    final_argument_whitespace = True

    def run(self):
        tags = self.arguments[0].split() if self.arguments else []
        node = tag_all_node('', '', tags=tags)
        return [node]

# .. fst
# ~~~~~~

class fst_node(nodes.literal_block):
    @staticmethod
    def visit(visitor, node):
        return visitor.visit_literal_block(node)
    @staticmethod
    def depart(visitor, node):
        return visitor.depart_literal_block(node)

class FStarBlockDirective(FStarListingBaseDirective):
    directive = "fst"
    node_class = fst_node

# .. exercise-code
# ~~~~~~~~~~~~~~~~

class exercise_code_node(nodes.literal_block):
    @staticmethod
    def visit(visitor, node):
        return visitor.visit_literal_block(node)
    @staticmethod
    def depart(visitor, node):
        return visitor.depart_literal_block(node)

class ExerciseCode(FStarListingBaseDirective):
    directive = "exercise-code"
    node_class = exercise_code_node

# Transforms
# ----------

class CheckExerciseSubNodesTransform(Transform):
    default_priority = 400

    def apply(self):
        for node in self.document.traverse(exercise_code_node):
            assert_attached_to(node, exercise_node)
        for node in self.document.traverse(solution_node):
            assert_attached_to(node, exercise_node)
        for node in self.document.traverse(standalone_editor_reference_node):
            assert_attached_to(node, exercise_node)

class ApplyTagsVisitor(nodes.GenericNodeVisitor):
    def __init__(self, document):
        nodes.GenericNodeVisitor.__init__(self, document)
        self.tags, self.tags_expiry_marker = [], None

    def visit_fst_node(self, node):
        node["tags"].extend(self.tags)

    def visit_tag_all_node(self, node):
        self.tags = node["tags"]
        self.tags_expiry_marker = node.parent

    def depart_tag_all_node(self, node): # pylint: disable=no-self-use
        node.parent.remove(node)

    def default_visit(self, _):
        pass

    def default_departure(self, node):
        if node is self.tags_expiry_marker:
            self.tags = []

class ApplyTagsTransform(Transform):
    default_priority = 401
    def apply(self):
        self.document.walkabout(ApplyTagsVisitor(self.document))

class ExerciseSnippetsVisitor(nodes.SparseNodeVisitor):
    def __init__(self, document):
        nodes.SparseNodeVisitor.__init__(self, document)
        self.code_blocks = [] # type: List[str]

    def visit_exercise_code_node(self, node):
        self.code_blocks.append(node)

    def visit_fst_node(self, node):
        self.code_blocks.append(node)

    def visit_solution_node(self, _): # pylint: disable=no-self-use
        raise nodes.StopTraversal()

class BuildFStarArtifactsTransform(Transform):
    default_priority = 402

    def apply(self):
        docname = self.document.settings.env.docname
        rootdir = self.document.settings.env.app.builder.outdir

        for cls in [exercise_node, solution_node]:
            mkdir(os.path.abspath(os.path.join(rootdir, cls.subdir)))

        code_blocks = []
        for node in self.document.traverse():
            if isinstance(node, fst_node):
                code_blocks.append(node)
            elif isinstance(node, exercise_node):
                if node.get("fname"):
                    visitor = ExerciseSnippetsVisitor(self.document)
                    node.walk(visitor)
                    # Write the artifact
                    node.artifact = Artifact(
                        docname, node["fname"], node.subdir,
                        [fst.lines for fst in node.filters.apply(code_blocks)],
                        [fst.lines for fst in visitor.code_blocks])
                    node.artifact.writeout(rootdir)
                    # Add a link
                    solution = next(iter(node.traverse(solution_node)), None)
                    if solution:
                        parent = solution.parent
                        index = parent.index(solution)
                        parent.insert(index, node.artifact.make_link())
            elif isinstance(node, solution_node):
                exercise = find_parent(node, exercise_node)
                if exercise.get("fname"):
                    node["fname"] = exercise["fname"] + ".Solution"
                    node.filters = exercise.filters
                    # Write the artifact
                    node.artifact = Artifact(
                        docname, node["fname"], node.subdir,
                        [fst.lines for fst in node.filters.apply(code_blocks)],
                        [fst.lines for fst in node.traverse(fst_node)])
                    node.artifact.writeout(rootdir)
                    # Add a link
                    node.append(node.artifact.make_link())

# :type:`…`
# ---------

def FStarTypeRole(typ, rawtext, text, lineno, inliner, options={}, content=[]):
    """An inline role to highlight F* types."""
    #pylint: disable=dangerous-default-value, unused-argument
    return nodes.literal(typ, rawtext, text, lineno, inliner, options=options, content=content)
FStarTypeRole.role = "type"

# FST parser
# ==========

# FIXME line numbers in error messages are wrong

class LiterateFStarParser(docutils.parsers.Parser):
    """A wrapper around the reStructuredText parser for F* literate files.
Line numbers are incorrectly reported, though."""

    supported = ('fst', 'fsti')
    """Aliases this parser supports."""

    settings_spec = ('Literate F* Parser Options', None, ())
    config_section = 'Literate F* parser'
    config_section_dependencies = ('parsers',)

    def __init__(self):
        self.reporter = None
        self.linemap = None # type: List[int]
        self.parser = docutils.parsers.rst.Parser()

    def get_transforms(self):
        return self.parser.get_transforms()

    @staticmethod
    def fst2rst(fst_string):
        linemap, rst_lines = zip(*fst2rst_linums(fst_string.splitlines(), None))
        return list(linemap), "\n".join(rst_lines)

    def parse(self, fst_string, document):
        """Parse `inputstring` and populate `document`, a document tree."""
        self.linemap, rst_string = LiterateFStarParser.fst2rst(fst_string)
        self.parser.parse(rst_string, document)

# FStar.js support
# ================

FSTAR_JS_CODA = """
<!-- F*.js configuration -->
<script>
__FSTAR_JS_CURRENT_FST_FILE_NAME__ = "{{{FILENAME}}}"
</script>
<!-- End of F*.js configuration -->
"""

def insert_fstarjs_script_tags(_app, doctree, fromdocname):
    js = FSTAR_JS_CODA.replace("{{{FILENAME}}}", fromdocname + ".fst")
    doctree.append(nodes.raw("", js, format="html"))

# Main entry point
# ================

def register():
    roles.register_local_role("type", nodes.literal)
    for directive in DIRECTIVES:
        directives.register_directive(directive.directive, directive)

# Index
# =====

ROLES = [FStarTypeRole]
NODES = [exercise_node, solution_node, fst_node, exercise_code_node,
         standalone_editor_reference_node]
DIRECTIVES = [FixmeAuthorsDirective, FixmeDirective, FStarBlockDirective,
              ExerciseDirective, SolutionDirective, ExerciseCode,
              TagAllDirective]
TRANSFORMS = [CheckExerciseSubNodesTransform, ApplyTagsTransform, BuildFStarArtifactsTransform]
