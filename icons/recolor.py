#!/usr/bin/env python3

import sys
from xml.dom import minidom

def get_style(node):
    style = node.attributes.get("style")
    if style:
        for kv in style.value.split(";"):
            # print(kv)
            yield kv.split(":", 2)

def set_style(node, style):
    node.attributes["style"] = ";".join(k + ":" + v for (k, v) in sorted(style.items()))

def set_fills(svg, color):
    for path in svg.getElementsByTagName('path') + svg.getElementsByTagName('rect'):
        style = dict(get_style(path))
        style["fill"] = color
        if style.get("stroke") not in [None, "none"]:
            style["stroke"] = color
        set_style(path, style)

SHADE_COLORS = {"dark": "#000000",
                "light": "#FFFFFF"}

def main():
    shade, in_path, out_path = sys.argv[1:]
    svg = minidom.parse(in_path)
    set_fills(svg, SHADE_COLORS[shade])
    with open(out_path, mode="w", encoding="utf-8") as writer:
        svg.writexml(writer)

if __name__ == '__main__':
    main()
