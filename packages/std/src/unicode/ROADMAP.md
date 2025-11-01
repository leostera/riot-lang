# Unicode Module Roadmap

## Current Status (v0.1 - December 2024)

**Size**: ~33 KB of implementation  
**Coverage**: ~90% of terminal text editing use cases  
**Production Ready**: ✅ Yes, for TextArea and terminal applications

### What's Implemented ✅

1. **Display Width Calculation** (~95% accurate)
   - Complete East Asian Width tables from Unicode 15.0
   - Binary search for O(log n) lookups
   - Handles: ASCII, CJK, Emoji, Combining marks, Fullwidth
   - Files: `width_tables.ml` (9 KB)

2. **Grapheme Cluster Breaking** (~90% accurate)
   - UAX #29 core rules (GB3-GB13)
   - Handles: Combining marks, ZWJ sequences, Regional Indicators, Hangul
   - Files: `grapheme_break.ml` (7 KB), `grapheme.ml` (3 KB)

3. **Word Boundary Detection** (~90% accurate)
   - Simplified UAX #29 word breaking
   - Handles: Latin scripts, CJK, Contractions, Identifiers, Numbers
   - Perfect for Ctrl+Arrow navigation
   - Files: `word_break.ml` (7 KB)

4. **UTF-8 Codec** (100% accurate)
   - Encoding/decoding
   - Validation
   - Files: `utf8.ml` (1 KB)

5. **Basic Character Classification**
   - ASCII/Latin-1: letters, digits, spaces, punctuation
   - Simple case conversion
   - Files: `rune.ml` (4 KB)

---

## Phase 1: Full Character Classification (Priority: Medium)

**Estimated Size**: +244 KB  
**Estimated Effort**: 2-3 days  
**Use Case**: Proper `is_letter`, `is_digit` for all Unicode scripts

### What's Missing

Currently, our character classification only works for ASCII and Latin-1:
```ocaml
Rune.is_letter 'A'  (* ✅ true *)
Rune.is_letter 'α'  (* ❌ false - should be true (Greek alpha) *)
Rune.is_letter 'А'  (* ❌ false - should be true (Cyrillic A) *)
Rune.is_letter '中' (* ❌ false - should be true (CJK) *)
```

### Tasks

- [ ] Port Unicode category tables from `golang.org/x/unicode/tables.go` (~9,768 lines)
  - Letter categories: Lu, Ll, Lt, Lm, Lo
  - Mark categories: Mn, Mc, Me
  - Number categories: Nd, Nl, No
  - Punctuation categories: Pc, Pd, Ps, Pe, Pi, Pf, Po
  - Symbol categories: Sm, Sc, Sk, So
  - Separator categories: Zs, Zl, Zp
  - Other categories: Cc, Cf, Cs, Co, Cn

- [ ] Implement binary search over RangeTable structure
  - Use R16 (16-bit ranges) and R32 (32-bit ranges) split
  - Optimize with LatinOffset for common Latin-1 fast path

- [ ] Update `Rune` module functions:
  - `is_letter` - full Unicode letter detection
  - `is_digit` - all Unicode digit systems
  - `is_space` - all Unicode whitespace
  - `is_mark` - all combining marks
  - `is_punct` - all punctuation
  - `is_symbol` - all symbols
  - `is_number` - all number categories

- [ ] Port case conversion tables from `casetables.go` (~755 lines)
  - Full Unicode case folding
  - Titlecase mappings
  - Special case handling (Turkish, Azeri)

- [ ] Add script detection
  - Common scripts: Latin, Greek, Cyrillic, Arabic, Hebrew, Thai, etc.
  - Script ranges and property tables

### Files to Create

```
packages/std/src/unicode/
├── unicode_tables.ml     (~244 KB) - Full category tables
├── case_tables.ml        (~1 KB)   - Case conversion tables
└── scripts.ml            (~50 KB)  - Script detection tables
```

### Testing

- [ ] Test all Unicode categories (L*, M*, N*, P*, S*, Z*, C*)
- [ ] Test case conversion for all scripts
- [ ] Test script detection
- [ ] Benchmark performance vs simplified version

### Breaking Changes

None - this is purely additive. Existing functions become more accurate.

---

## Phase 2: Line Breaking Algorithm (Priority: Low-Medium)

**Estimated Size**: +140 KB  
**Estimated Effort**: 3-4 days  
**Use Case**: Proper text wrapping in terminals

### What's Missing

Currently, we only break on newlines:
```ocaml
(* Current: breaks only on \n *)
find_line_breaks "Hello\nWorld"  (* Works ✅ *)
find_line_breaks "Long text that should wrap"  (* Doesn't wrap ❌ *)
```

### Tasks

- [ ] Port line break property tables from `uniseg/lineproperties.go` (~3,554 lines)
  - 40+ line break properties (BK, CR, LF, CM, NL, SG, WJ, ZW, GL, SP, ...)
  - Complex property lookup tables

- [ ] Implement UAX #14 Line Breaking Algorithm
  - Port rules from `uniseg/linerules.go` (~626 lines)
  - State machine with 100+ transition rules
  - Lookahead for complex breaks

- [ ] Handle special cases:
  - Non-breaking spaces
  - Word joiner characters
  - Zero-width spaces
  - East Asian quotation marks
  - Combining marks after break opportunities

- [ ] Update `Segmentation.find_line_breaks`
  - Return `Must_break`, `Can_break`, `Dont_break` properly
  - Consider East Asian context

- [ ] Add text wrapping helpers to `String` module
  - Wrap at line break opportunities
  - Respect minimum/maximum word lengths
  - Handle forced breaks vs. optional breaks

### Files to Create

```
packages/std/src/unicode/
├── line_break_properties.ml  (~140 KB) - Property tables
├── line_break_rules.ml       (~5 KB)   - UAX #14 algorithm
└── line_break.ml             (~2 KB)   - Main API
```

### Use Cases

- Text wrapping in terminal UIs
- Pagination
- Pretty printing
- Log formatting

---

## Phase 3: Sentence Segmentation (Priority: Low)

**Estimated Size**: +110 KB  
**Estimated Effort**: 2-3 days  
**Use Case**: Natural language processing, "jump to next sentence"

### What's Missing

Currently, we only break on `.!?`:
```ocaml
(* Current: naive punctuation detection *)
find_sentence_boundaries "Dr. Smith is here."  
(* Breaks after "Dr." incorrectly ❌ *)
```

### Tasks

- [ ] Port sentence break property tables from `uniseg/sentenceproperties.go` (~2,845 lines)
  - ATerm, STerm, Close, Sp, Sep, Format, Extend, etc.
  - Abbreviation detection
  - Quote handling

- [ ] Implement UAX #29 Sentence Boundary Algorithm
  - Port rules from `uniseg/sentencerules.go` (~276 lines)
  - Handle abbreviations (Dr., Mr., etc.)
  - Handle quotations
  - Handle ellipsis (...)

- [ ] Update `Segmentation.find_sentence_boundaries`
  - Proper sentence detection
  - Multi-language support

### Files to Create

```
packages/std/src/unicode/
├── sentence_properties.ml  (~110 KB) - Property tables
└── sentence_rules.ml       (~3 KB)   - UAX #29 algorithm
```

### Use Cases

- Natural language processing
- Text analysis
- Navigation (Alt+Arrow for sentence jumping)
- Text-to-speech boundary detection

---

## Phase 4: Advanced Features (Priority: Very Low)

These are nice-to-have but not critical for terminal text editing.

### Unicode Normalization

**Size**: ~100 KB  
**Effort**: 4-5 days

- [ ] NFC (Canonical Decomposition + Canonical Composition)
- [ ] NFD (Canonical Decomposition)
- [ ] NFKC (Compatibility Decomposition + Canonical Composition)
- [ ] NFKD (Compatibility Decomposition)

**Use Cases**:
- Text comparison (treating "café" and "café" as equal)
- Filesystem paths (macOS uses NFD, Linux uses NFC)
- Database storage
- Search/indexing

### Collation (Sorting)

**Size**: ~500 KB  
**Effort**: 1-2 weeks

- [ ] Unicode Collation Algorithm (UCA)
- [ ] Locale-specific sorting
- [ ] Case-insensitive comparison
- [ ] Accent-insensitive comparison

**Use Cases**:
- Sorting multilingual text
- Alphabetical ordering
- Search results ranking

### Bidirectional Text (Bidi)

**Size**: ~50 KB  
**Effort**: 1 week

- [ ] UAX #9 Bidirectional Algorithm
- [ ] RTL (Right-to-Left) support
- [ ] LTR (Left-to-Right) support
- [ ] Mixed directionality

**Use Cases**:
- Arabic, Hebrew, Persian text
- Mixed LTR/RTL documents
- Terminal UI for RTL languages

### Full Word Breaking

**Size**: ~75 KB  
**Effort**: 1-2 days

- [ ] Complete UAX #29 word property tables (~1,883 lines)
- [ ] All word break rules with lookahead
- [ ] Hebrew double quote handling
- [ ] Complex Indic script handling

**Use Cases**:
- 100% accurate word detection for all scripts
- Complex language support (Thai, Lao, Myanmar)
- Linguistic analysis

---

## Size Comparison Summary

| Component | Current | Phase 1 | Phase 2 | Phase 3 | Phase 4 | Total |
|-----------|---------|---------|---------|---------|---------|-------|
| Core (width, grapheme, word, utf8) | 33 KB | - | - | - | - | 33 KB |
| Character classification | - | 244 KB | - | - | - | 244 KB |
| Line breaking | - | - | 140 KB | - | - | 140 KB |
| Sentence segmentation | - | - | - | 110 KB | - | 110 KB |
| Advanced features | - | - | - | - | 650 KB | 650 KB |
| **Total** | **33 KB** | **277 KB** | **417 KB** | **527 KB** | **1,177 KB** | **1,177 KB** |

**Current state**: 3% of full Unicode implementation  
**With Phase 1**: 24% of full implementation  
**With Phases 1-3**: 45% of full implementation  
**Complete**: 100% of full implementation (~1.2 MB)

---

## Recommendations

### For TextArea (Terminal Text Editor)

**Current implementation is sufficient! ✅**

What you have:
- ✅ Display width (critical for cursor positioning)
- ✅ Grapheme clustering (emoji, combining marks)
- ✅ Word navigation (Ctrl+Arrow)
- ✅ UTF-8 codec

What you might want:
- 🤔 **Phase 1** (Full character classification) - Only if validating non-Latin input
- 🤔 **Phase 2** (Line breaking) - Only if implementing automatic text wrapping

What you don't need:
- ❌ Phase 3 (Sentence segmentation)
- ❌ Phase 4 (Advanced features)

### For General Text Processing

Consider implementing phases based on requirements:
- **Web applications**: Phase 1 + Phase 4 (Normalization, Bidi)
- **Text editors**: Current + Phase 1 + Phase 2
- **NLP/Search**: Current + Phase 1 + Phase 3 + Phase 4 (Collation, Normalization)
- **Terminal UIs**: **Current state is perfect!**

---

## Contributing

If you want to implement any of these phases:

1. Create an issue for the specific phase
2. Discuss the approach and scope
3. Consider the size/performance trade-offs
4. Test thoroughly with Unicode test data
5. Update benchmarks
6. Document the new capabilities

## References

- [UAX #29: Unicode Text Segmentation](https://www.unicode.org/reports/tr29/)
- [UAX #14: Unicode Line Breaking Algorithm](https://www.unicode.org/reports/tr14/)
- [UAX #9: Unicode Bidirectional Algorithm](https://www.unicode.org/reports/tr9/)
- [UAX #15: Unicode Normalization Forms](https://www.unicode.org/reports/tr15/)
- [UCA: Unicode Collation Algorithm](https://www.unicode.org/reports/tr10/)
- [Unicode 15.0 Data Files](https://www.unicode.org/Public/15.0.0/)
