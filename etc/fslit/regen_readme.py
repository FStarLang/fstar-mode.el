#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""Rebuild README.rst from README.template.rst."""

from os import sys, path

script_dir = path.dirname(path.abspath(__file__))
if __name__ == "__main__" and __package__ is None:
    sys.path.append(path.dirname(script_dir))

import fslit.docutils4fstar
from codecs import open

README_DIRECTIVES_MARKER = "[DIRECTIVES]"
README_ROLES_MARKER = "[ROLES]"

def regen_readme():
    directives_docs = "\n\n".join("``.. {}::`` {}".format(d.directive, d.__doc__.strip())
                                  for d in fslit.docutils4fstar.DIRECTIVES)
    roles_docs = "\n\n".join("``:{}:`` {}".format(r.role, r.__doc__.strip())
                             for r in fslit.docutils4fstar.ROLES)
    with open(path.join(script_dir, "README.template.rst"), encoding="utf-8") as readme:
        contents = readme.read()
    with open(path.join(script_dir, "README.rst"), mode="w", encoding="utf-8") as readme:
        readme.write(contents
                     .replace(README_ROLES_MARKER, roles_docs)
                     .replace(README_DIRECTIVES_MARKER, directives_docs))

if __name__ == '__main__':
    regen_readme()
