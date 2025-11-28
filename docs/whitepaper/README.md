# MiniRiot Whitepaper

This directory contains the LaTeX source for the MiniRiot whitepaper, targeting submission to ICFP (International Conference on Functional Programming).

## Paper Title
"MiniRiot: Bringing Erlang's Actor Model to OCaml through Effect Handlers"

## Abstract Summary
The paper presents MiniRiot, a lightweight actor-model runtime for OCaml that leverages OCaml 5's effect handlers to implement Erlang-style processes and message passing. Key contributions include:

1. Effect-based process abstraction for lightweight concurrency
2. Type-safe message passing using extensible variants  
3. Selective receive with save queues via effect handlers
4. Hierarchical timing wheels for efficient timer management
5. A gradual scaling path from single-core to multi-core

## Building the Paper

### Prerequisites
- A LaTeX distribution (TeXLive, MacTeX, or MiKTeX)
- The ACM article class (`acmart.cls`)
- Standard LaTeX packages

### Build Commands

```bash
# Build the PDF
make

# Build and view the PDF
make view

# Clean build artifacts
make clean

# Watch for changes and auto-rebuild
make watch
```

## Paper Structure

- **Section 1: Introduction** - Motivation and contributions
- **Section 2: Background** - Actor model, Erlang/OTP, and effect handlers
- **Section 3: System Design** - Architecture and design principles
- **Section 4: Implementation** - Technical details of key components
- **Section 5: Evaluation** - Performance benchmarks and case studies
- **Section 6: Related Work** - Comparison with other systems
- **Section 7: Conclusion** - Summary and future work

## Key Technical Highlights

### Effect Handlers Enable Actor Model
The paper emphasizes how OCaml 5's effect handlers are the key enabler, providing:
- Lightweight process suspension/resumption without manual CPS
- Direct-style programming (vs monadic style)
- Efficient cooperative scheduling

### Novel Contributions
1. **First actor system using OCaml 5 effect handlers** - Demonstrates a new application of this language feature
2. **Type-safe extensible messaging** - Combines static typing with dynamic flexibility
3. **Functional timing wheels** - Adapts systems techniques to functional programming
4. **Migration path** - Shows how to scale from simple single-core to complex multi-core

## Submission Target

**Conference**: ICFP 2024/2025
**Track**: Research Papers
**Page Limit**: 12 pages (excluding references)

## TODO for Submission

- [ ] Add performance comparison graphs
- [ ] Include more detailed benchmarks
- [ ] Add case study results
- [ ] Review related work section for completeness
- [ ] Get feedback from OCaml and Erlang communities
- [ ] Professional proofreading
- [ ] Ensure all code examples compile
- [ ] Add artifact evaluation package

## Code Availability

The MiniRiot implementation is available at:
- Repository: https://github.com/riot-ml/riot
- Path: `/packages/miniriot/`

## Authors Note

This is a first draft. The paper needs:
1. Real benchmark data and graphs
2. More detailed evaluation section
3. Comparison with other actor systems
4. Discussion of limitations
5. More thorough related work analysis

## Citation

If you use this work, please cite:

```bibtex
@inproceedings{miniriot2024,
  author = {Leandro Ostera},
  title = {MiniRiot: Bringing Erlang's Actor Model to OCaml through Effect Handlers},
  booktitle = {Proceedings of ICFP 2024},
  year = {2024},
  publisher = {ACM}
}
```
