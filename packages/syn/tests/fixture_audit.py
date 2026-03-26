#!/usr/bin/env python3

import argparse
import hashlib
import re
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional


@dataclass(frozen=True)
class Fixture:
    path: Path
    source: str
    expected_lossless: Optional[str]
    expected_cst: Optional[str]

    @property
    def name(self) -> str:
        return self.path.name


def fixture_dir() -> Path:
    return Path(__file__).resolve().parent / "fixtures"


def load_fixtures() -> List[Fixture]:
    fixtures: List[Fixture] = []
    for path in sorted(fixture_dir().iterdir()):
        if not path.is_file():
            continue
        if path.suffix not in {".ml", ".mli"}:
            continue
        if path.name.endswith(".expected_lossless.json") or path.name.endswith(".expected_cst.json"):
            continue

        lossless_expected = read_optional_text(Path(f"{path}.expected_lossless.json"))
        cst_expected = read_optional_text(Path(f"{path}.expected_cst.json"))
        fixtures.append(
            Fixture(
                path=path,
                source=path.read_text(),
                expected_lossless=lossless_expected,
                expected_cst=cst_expected,
            )
        )
    return fixtures


def read_optional_text(path: Path) -> Optional[str]:
    return path.read_text() if path.exists() else None


def category_for(name: str) -> str:
    lower = name.lower()
    if any(token in lower for token in ["trivia", "comment", "docstring", "mixed"]):
        return "trivia"
    if any(token in lower for token in ["module", "sig", "functor", "open", "fcm", "first_class", "pack", "include"]):
        return "module-and-signatures"
    if any(token in lower for token in ["object", "method", "class", "new_"]):
        return "objects"
    if any(token in lower for token in ["record", "array", "list", "tuple", "field", "index"]):
        return "data-structures"
    if any(token in lower for token in ["match", "function", "pattern", "guard"]):
        return "functions-and-patterns"
    if any(token in lower for token in ["if", "let", "seq", "while", "for", "try", "begin", "lazy", "assert"]):
        return "bindings-and-control-flow"
    if any(token in lower for token in ["type", "val", "external"]):
        return "types-and-signatures"
    if any(token in lower for token in ["ocaml_"]):
        return "upstream"
    return "atoms"


def hash_text(text: str) -> str:
    return hashlib.sha256(text.encode()).hexdigest()


def text_for(fixture: Fixture, field: str) -> Optional[str]:
    if field == "source":
        return fixture.source
    if field == "lossless":
        return fixture.expected_lossless
    if field == "cst":
        return fixture.expected_cst
    if field == "pair":
        if fixture.expected_lossless is None or fixture.expected_cst is None:
            return None
        return fixture.source + "\0" + fixture.expected_lossless + "\0" + fixture.expected_cst
    raise ValueError(f"unsupported fixture field: {field}")


def group_by(fixtures: Iterable[Fixture], field: str) -> List[List[str]]:
    groups: Dict[str, List[str]] = defaultdict(list)
    for fixture in fixtures:
        text = text_for(fixture, field)
        if text is None:
            continue
        groups[hash_text(text)].append(fixture.name)
    return sorted((sorted(names) for names in groups.values() if len(names) > 1), key=lambda names: (-len(names), names[0]))


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
            replace_cost = previous[j - 1] + (char_a != char_b)
            current.append(min(insert_cost, delete_cost, replace_cost))
        previous = current
    return previous[-1]


def similarity_score(a: str, b: str) -> float:
    max_len = max(len(a), len(b))
    if max_len == 0:
        return 1.0
    return 1.0 - (levenshtein_distance(a, b) / max_len)


def near_duplicate_groups(fixtures: List[Fixture], *, field: str, threshold: float) -> List[List[str]]:
    grouped: Dict[str, List[Fixture]] = defaultdict(list)
    for fixture in fixtures:
        fixture_text = text_for(fixture, field)
        if fixture_text is None:
            continue
        grouped[category_for(fixture.name)].append(fixture)

    adjacency: Dict[str, set[str]] = defaultdict(set)
    for bucket in grouped.values():
        for i, fixture_a in enumerate(bucket):
            text_a = text_for(fixture_a, field)
            if text_a is None:
                continue
            for fixture_b in bucket[i + 1 :]:
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
    groups: List[List[str]] = []
    for root in sorted(adjacency):
        if root in seen:
            continue
        stack = [root]
        component: List[str] = []
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
    numeric_ids: Dict[str, List[str]] = defaultdict(list)

    for fixture in fixtures:
        categories[category_for(fixture.name)].append(fixture.name)
        match = re.match(r"^(\d{3,4})_", fixture.name)
        if match:
            numeric_ids[match.group(1)].append(fixture.name)

    print("Category summary")
    for category in sorted(categories):
        names = categories[category]
        print(f"- {category}: {len(names)} fixtures")

    collisions = {k: v for k, v in numeric_ids.items() if len(v) > 1}
    print(f"Numeric fixture id collisions: {len(collisions)}")
    for numeric_id in sorted(collisions):
        print(f"- {numeric_id}: {', '.join(sorted(collisions[numeric_id]))}")


def print_duplicate_groups(fixtures: List[Fixture], kind: str, limit: int) -> None:
    groups = group_by(fixtures, kind)
    print(f"\nExact duplicate {kind} groups: {len(groups)}")
    for names in groups[:limit]:
        print(f"- ({len(names)}) " + ", ".join(names))


def print_near_duplicate_groups(fixtures: List[Fixture], *, field: str, threshold: float, limit: int) -> None:
    groups = near_duplicate_groups(fixtures, field=field, threshold=threshold)
    print(f"\nNear-duplicate {field} groups at >= {threshold:.2f} normalized similarity: {len(groups)}")
    for names in groups[:limit]:
        print(f"- ({len(names)}) " + ", ".join(names))


def print_deletion_plan(source_groups: List[List[str]]) -> None:
    removed = []
    kept = 0
    for group in source_groups:
        if len(group) <= 1:
            continue
        kept += 1
        kept_name = sorted(group)[0]
        removed.extend(sorted(group)[1:])

    print(f"\nSuggested deletion plan: keep {kept} canonical fixture(s), remove {len(removed)} duplicates")
    if removed:
        print("- " + ", ".join(removed))


def print_deletion_list(source_groups: List[List[str]]) -> None:
    removed = []
    for group in source_groups:
        if len(group) <= 1:
            continue
        removed.extend(sorted(group)[1:])
    for item in removed:
        print(item)


def main() -> int:
    parser = argparse.ArgumentParser(description="Audit syn fixture corpus for duplicates")
    parser.add_argument(
        "--duplicates",
        choices=["source", "lossless", "cst", "pair", "all"],
        default="all",
        help="Which exact-duplicate buckets to print",
    )
    parser.add_argument("--limit", type=int, default=20, help="Max duplicate groups to print for each bucket")
    parser.add_argument(
        "--near-field",
        choices=["source", "lossless", "cst", "pair"],
        default="source",
        help="Which fixture text to compare for near-duplicate grouping",
    )
    parser.add_argument(
        "--near-threshold",
        type=float,
        default=0.88,
        help="Normalized Levenshtein similarity threshold for near-duplicate families",
    )
    parser.add_argument(
        "--skip-near-duplicates",
        action="store_true",
        help="Skip near-duplicate grouping and print only exact duplicates",
    )
    parser.add_argument(
        "--include-deletion-plan",
        action="store_true",
        help="Print a first-pass deletion plan for exact source duplicates",
    )
    parser.add_argument(
        "--print-delete-list",
        action="store_true",
        help="Print one file per line for source duplicates to delete (keeps first in each group)",
    )

    args = parser.parse_args()

    fixtures = load_fixtures()
    print(f"Fixtures: {len(fixtures)}")
    print_category_summary(fixtures)

    kinds = ["source", "lossless", "cst", "pair"] if args.duplicates == "all" else [args.duplicates]
    for kind in kinds:
        print_duplicate_groups(fixtures, kind, args.limit)

    if args.include_deletion_plan and "source" in kinds:
        source_groups = group_by(fixtures, "source")
        print_deletion_plan(source_groups)
    elif args.print_delete_list and "source" in kinds:
        source_groups = group_by(fixtures, "source")
        print_deletion_list(source_groups)

    if not args.skip_near_duplicates:
        print_near_duplicate_groups(fixtures, field=args.near_field, threshold=args.near_threshold, limit=args.limit)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
