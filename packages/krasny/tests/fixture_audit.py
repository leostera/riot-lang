#!/usr/bin/env python3

import argparse
import hashlib
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List


@dataclass(frozen=True)
class Fixture:
    path: Path
    source: str
    expected: str | None

    @property
    def name(self) -> str:
        return self.path.name


def fixture_dir() -> Path:
    return Path(__file__).resolve().parent / "fixtures"


def load_fixtures() -> List[Fixture]:
    fixtures = []
    for path in sorted(fixture_dir().iterdir()):
        if not path.is_file():
            continue
        if path.suffix not in {".ml", ".mli"}:
            continue
        if path.name.endswith(".expected"):
            continue
        expected_path = Path(str(path) + ".expected")
        expected = expected_path.read_text() if expected_path.exists() else None
        fixtures.append(Fixture(path=path, source=path.read_text(), expected=expected))
    return fixtures


def category_for(name: str) -> str:
    lower = name.lower()
    if any(token in lower for token in ["comment", "docstring", "trivia", "mixed_supported"]):
        return "trivia-and-mixed-top-level"
    if any(
        token in lower
        for token in [
            "module",
            "sig",
            "functor",
            "include",
            "open",
            "fcm",
            "first_class",
            "pack",
        ]
    ):
        return "modules-and-signatures"
    if any(token in lower for token in ["object", "method", "class", "instance", "new_"]):
        return "objects-and-methods"
    if any(token in lower for token in ["type_", "val_", "external", "module_type"]):
        return "types-and-signatures"
    if any(
        token in lower
        for token in [
            "record",
            "array",
            "list",
            "tuple",
            "field",
            "update",
            "string_index",
            "array_index",
        ]
    ):
        return "data-structures-and-updates"
    if any(token in lower for token in ["match", "function", "pattern", "guard"]):
        return "functions-and-pattern-matching"
    if any(
        token in lower
        for token in ["if", "let", "seq", "while", "for", "try", "begin", "lazy", "assert"]
    ):
        return "control-flow-and-bindings"
    return "atoms-and-basic-expressions"


def hash_text(text: str) -> str:
    return hashlib.sha256(text.encode()).hexdigest()


def text_for(fixture: Fixture, field: str) -> str | None:
    if field == "source":
        return fixture.source
    if field == "expected":
        return fixture.expected
    if field == "pair":
        if fixture.expected is None:
            return None
        return fixture.source + "\0" + fixture.expected
    raise ValueError(f"unsupported fixture text field: {field}")


def group_by(fixtures: Iterable[Fixture], kind: str) -> List[List[str]]:
    groups: Dict[str, List[str]] = defaultdict(list)
    for fixture in fixtures:
        text = text_for(fixture, kind)
        if text is None:
            continue
        digest = hash_text(text)
        groups[digest].append(fixture.name)
    duplicates = [sorted(names) for names in groups.values() if len(names) > 1]
    return sorted(duplicates, key=lambda names: (-len(names), names[0]))


def levenshtein_distance(a: str, b: str) -> int:
    if a == b:
        return 0
    if len(a) < len(b):
        a, b = b, a
    previous = list(range(len(b) + 1))
    for i, char_a in enumerate(a, 1):
        current = [i]
        for j, char_b in enumerate(b, 1):
            insert_cost = current[j - 1] + 1
            delete_cost = previous[j] + 1
            replace_cost = previous[j - 1]
            if char_a != char_b:
                replace_cost += 1
            current.append(min(insert_cost, delete_cost, replace_cost))
        previous = current
    return previous[-1]


def similarity_score(a: str, b: str) -> float:
    max_len = max(len(a), len(b))
    if max_len == 0:
        return 1.0
    return 1.0 - (levenshtein_distance(a, b) / max_len)


def near_duplicate_groups(
    fixtures: List[Fixture],
    *,
    field: str,
    threshold: float,
) -> List[List[str]]:
    grouped: Dict[str, List[Fixture]] = defaultdict(list)
    for fixture in fixtures:
        if text_for(fixture, field) is None:
            continue
        grouped[category_for(fixture.name)].append(fixture)

    adjacency: Dict[str, set[str]] = defaultdict(set)
    for bucket in grouped.values():
        for index, fixture_a in enumerate(bucket):
            text_a = text_for(fixture_a, field)
            if text_a is None:
                continue
            for fixture_b in bucket[index + 1 :]:
                text_b = text_for(fixture_b, field)
                if text_b is None:
                    continue
                max_len = max(len(text_a), len(text_b))
                if max_len == 0:
                    continue
                if abs(len(text_a) - len(text_b)) > max_len * (1.0 - threshold):
                    continue
                if similarity_score(text_a, text_b) >= threshold:
                    adjacency[fixture_a.name].add(fixture_b.name)
                    adjacency[fixture_b.name].add(fixture_a.name)

    seen = set()
    groups = []
    for root in sorted(adjacency):
        if root in seen:
            continue
        stack = [root]
        component = []
        while stack:
            name = stack.pop()
            if name in seen:
                continue
            seen.add(name)
            component.append(name)
            stack.extend(adjacency[name] - seen)
        if len(component) > 1:
            groups.append(sorted(component))

    return sorted(groups, key=lambda names: (-len(names), names[0]))


def print_category_summary(fixtures: List[Fixture]) -> None:
    categories: Dict[str, List[str]] = defaultdict(list)
    for fixture in fixtures:
        categories[category_for(fixture.name)].append(fixture.name)

    print("Category summary")
    for category in sorted(categories):
        names = categories[category]
        preview = ", ".join(names[:6])
        if len(names) > 6:
            preview += ", ..."
        print(f"- {category}: {len(names)} fixtures")
        print(f"  {preview}")


def print_duplicate_groups(fixtures: List[Fixture], kind: str, limit: int) -> None:
    groups = group_by(fixtures, kind)
    print(f"\nExact duplicate {kind} groups: {len(groups)}")
    for names in groups[:limit]:
        print(f"- ({len(names)}) " + ", ".join(names))


def print_near_duplicate_groups(
    fixtures: List[Fixture],
    *,
    field: str,
    threshold: float,
    limit: int,
) -> None:
    groups = near_duplicate_groups(fixtures, field=field, threshold=threshold)
    print(
        f"\nNear-duplicate {field} groups at >= {threshold:.2f} normalized similarity: {len(groups)}"
    )
    for names in groups[:limit]:
        print(f"- ({len(names)}) " + ", ".join(names))


def main() -> int:
    parser = argparse.ArgumentParser(description="Audit krasny fixture categories and duplicates")
    parser.add_argument(
        "--duplicates",
        choices=["source", "expected", "pair", "all"],
        default="all",
        help="Which exact-duplicate buckets to print",
    )
    parser.add_argument(
        "--limit",
        type=int,
        default=25,
        help="Maximum duplicate groups to print for each bucket",
    )
    parser.add_argument(
        "--near-field",
        choices=["source", "expected", "pair"],
        default="source",
        help="Which fixture text to compare for near-duplicate families",
    )
    parser.add_argument(
        "--near-threshold",
        type=float,
        default=0.88,
        help="Normalized Levenshtein similarity threshold for near-duplicate grouping",
    )
    parser.add_argument(
        "--skip-near-duplicates",
        action="store_true",
        help="Skip near-duplicate grouping and print only exact duplicates",
    )
    args = parser.parse_args()

    fixtures = load_fixtures()
    print(f"Fixtures: {len(fixtures)}")
    print_category_summary(fixtures)

    kinds = ["source", "expected", "pair"] if args.duplicates == "all" else [args.duplicates]
    for kind in kinds:
        print_duplicate_groups(fixtures, kind, args.limit)
    if not args.skip_near_duplicates:
        print_near_duplicate_groups(
            fixtures,
            field=args.near_field,
            threshold=args.near_threshold,
            limit=args.limit,
        )

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
