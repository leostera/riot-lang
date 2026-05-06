#!/usr/bin/env python3
"""Summarize macOS xctrace Time Profiler XML exports.

Input must be the XML produced by:

  xcrun xctrace export \
    --input profile.trace \
    --xpath '/trace-toc/run[@number="1"]/data/table[@schema="time-profile"]'
"""

from __future__ import annotations

import argparse
from collections import defaultdict
import sys
import xml.etree.ElementTree as ET


def tag_name(element: ET.Element) -> str:
    return element.tag.rsplit("}", 1)[-1]


def parse_int(text: str | None) -> int:
    if text is None:
        return 0
    try:
        return int(text.strip())
    except ValueError:
        return 0


def ms(ns: int) -> float:
    return ns / 1_000_000.0


class XctraceXml:
    def __init__(self, root: ET.Element) -> None:
        self.root = root
        self.by_id: dict[str, ET.Element] = {}
        for element in root.iter():
            id_ = element.attrib.get("id")
            if id_ is not None:
                self.by_id[id_] = element

    def resolve(self, element: ET.Element | None) -> ET.Element | None:
        if element is None:
            return None
        ref = element.attrib.get("ref")
        if ref is None:
            return element
        return self.by_id.get(ref)

    def child(self, element: ET.Element | None, name: str) -> ET.Element | None:
        element = self.resolve(element)
        if element is None:
            return None
        for child in list(element):
            resolved = self.resolve(child)
            if resolved is not None and tag_name(resolved) == name:
                return resolved
        return None

    def rows(self) -> list[ET.Element]:
        return [element for element in self.root.iter() if tag_name(element) == "row"]

    def frame_name(self, frame: ET.Element, *, show_binary: bool) -> str:
        resolved = self.resolve(frame)
        if resolved is not None:
            frame = resolved
        name = frame.attrib.get("name") or frame.attrib.get("addr") or "<unknown>"
        if not show_binary:
            return name

        binary = self.child(frame, "binary")
        binary_name = binary.attrib.get("name") if binary is not None else None
        if binary_name is None:
            return name
        return f"{name} [{binary_name}]"

    def row_weight_ns(self, row: ET.Element) -> int:
        weight = self.child(row, "weight")
        if weight is None:
            return 0
        return parse_int(weight.text)

    def row_stack(self, row: ET.Element, *, show_binary: bool) -> list[str]:
        tagged_backtrace = self.child(row, "tagged-backtrace")
        backtrace = self.child(tagged_backtrace, "backtrace")
        if backtrace is None:
            return []

        frames: list[str] = []
        for child in list(backtrace):
            frame = self.resolve(child)
            if frame is not None and tag_name(frame) == "frame":
                frames.append(self.frame_name(frame, show_binary=show_binary))
        return frames


def load_xml(path: str) -> XctraceXml:
    if path == "-":
        root = ET.fromstring(sys.stdin.buffer.read())
    else:
        root = ET.parse(path).getroot()
    return XctraceXml(root)


def collect_samples(trace: XctraceXml, *, show_binary: bool) -> list[tuple[int, list[str]]]:
    samples: list[tuple[int, list[str]]] = []
    for row in trace.rows():
        weight_ns = trace.row_weight_ns(row)
        stack = trace.row_stack(row, show_binary=show_binary)
        if weight_ns > 0 and stack:
            samples.append((weight_ns, stack))
    return samples


def print_table(samples: list[tuple[int, list[str]]], *, limit: int, sort: str) -> None:
    total_cpu_ns = sum(weight_ns for weight_ns, _stack in samples)
    self_ns: dict[str, int] = defaultdict(int)
    total_ns: dict[str, int] = defaultdict(int)
    self_samples: dict[str, int] = defaultdict(int)

    for weight_ns, stack in samples:
        leaf = stack[0]
        self_ns[leaf] += weight_ns
        self_samples[leaf] += 1

        seen: set[str] = set()
        for frame in stack:
            if frame not in seen:
                total_ns[frame] += weight_ns
                seen.add(frame)

    key = self_ns if sort == "self" else total_ns
    names = sorted(total_ns.keys(), key=lambda name: key.get(name, 0), reverse=True)

    print(f"{'%cpu':>7} {'self_ms':>10} {'total_ms':>10} {'samples':>8}  function")
    print(f"{'-' * 7} {'-' * 10} {'-' * 10} {'-' * 8}  {'-' * 40}")
    for name in names[:limit]:
        basis = key.get(name, 0)
        pct = (basis / total_cpu_ns * 100.0) if total_cpu_ns > 0 else 0.0
        print(
            f"{pct:6.2f}% "
            f"{ms(self_ns.get(name, 0)):10.2f} "
            f"{ms(total_ns.get(name, 0)):10.2f} "
            f"{self_samples.get(name, 0):8d}  "
            f"{name}"
        )


class TreeNode:
    def __init__(self, name: str) -> None:
        self.name = name
        self.self_ns = 0
        self.total_ns = 0
        self.children: dict[str, TreeNode] = {}

    def child(self, name: str) -> "TreeNode":
        node = self.children.get(name)
        if node is None:
            node = TreeNode(name)
            self.children[name] = node
        return node


def build_tree(samples: list[tuple[int, list[str]]]) -> TreeNode:
    root = TreeNode("<root>")
    for weight_ns, stack in samples:
        root.total_ns += weight_ns
        node = root
        for frame in reversed(stack):
            node = node.child(frame)
            node.total_ns += weight_ns
        node.self_ns += weight_ns
    return root


def print_tree_node(
    node: TreeNode,
    *,
    root_total_ns: int,
    prefix: str,
    is_last: bool,
    depth: int,
    max_depth: int,
    max_children: int,
) -> None:
    if depth > max_depth:
        return

    branch = "`-- " if is_last else "|-- "
    pct = (node.total_ns / root_total_ns * 100.0) if root_total_ns > 0 else 0.0
    print(
        f"{prefix}{branch}"
        f"{pct:6.2f}% "
        f"total={ms(node.total_ns):8.2f}ms "
        f"self={ms(node.self_ns):8.2f}ms "
        f"{node.name}"
    )

    children = sorted(node.children.values(), key=lambda child: child.total_ns, reverse=True)
    shown = children[:max_children]
    next_prefix = prefix + ("    " if is_last else "|   ")
    for index, child in enumerate(shown):
        print_tree_node(
            child,
            root_total_ns=root_total_ns,
            prefix=next_prefix,
            is_last=index == len(shown) - 1 and len(children) <= max_children,
            depth=depth + 1,
            max_depth=max_depth,
            max_children=max_children,
        )

    hidden = len(children) - len(shown)
    if hidden > 0:
        print(f"{next_prefix}`-- ... {hidden} more")


def print_tree(samples: list[tuple[int, list[str]]], *, max_depth: int, max_children: int) -> None:
    root = build_tree(samples)
    print(f"total_cpu_ms={ms(root.total_ns):.2f}")
    children = sorted(root.children.values(), key=lambda child: child.total_ns, reverse=True)
    for index, child in enumerate(children[:max_children]):
        print_tree_node(
            child,
            root_total_ns=root.total_ns,
            prefix="",
            is_last=index == len(children) - 1,
            depth=1,
            max_depth=max_depth,
            max_children=max_children,
        )

    hidden = len(children) - max_children
    if hidden > 0:
        print(f"`-- ... {hidden} more")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("xml", help="xctrace time-profile XML file, or - for stdin")
    parser.add_argument("--mode", choices=["table", "tree"], default="table")
    parser.add_argument("--sort", choices=["self", "total"], default="self")
    parser.add_argument("--limit", type=int, default=40, help="rows to print in table mode")
    parser.add_argument("--depth", type=int, default=10, help="maximum tree depth")
    parser.add_argument("--children", type=int, default=12, help="children shown per tree node")
    parser.add_argument("--show-binary", action="store_true", help="include binary name in frame labels")
    args = parser.parse_args()

    trace = load_xml(args.xml)
    samples = collect_samples(trace, show_binary=args.show_binary)

    if args.mode == "table":
        print_table(samples, limit=args.limit, sort=args.sort)
    else:
        print_tree(samples, max_depth=args.depth, max_children=args.children)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
