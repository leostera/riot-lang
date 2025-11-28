# RIOT Papers and Thesis

This directory contains academic papers about RIOT:

## 📄 Documents Available

### 1. **ICFP 2-Page Extended Abstract** (`icfp-2page.pdf`)
- **Exactly 2 pages** for ICFP submission
- Concise presentation of core ideas
- Emphasizes effect handlers as the key enabler
- Ready for conference submission

### 2. **Master's Thesis** (`thesis.pdf`)
- **54 pages** comprehensive document
- Full technical depth suitable for master's thesis
- Detailed implementation and evaluation
- Complete analysis of design decisions

### 3. **Extended Paper** (`paper-thesis.pdf`)
- 6-page version with more details than ICFP
- Good for journal submission or technical report

## 📊 Thesis Structure (54 pages)

1. **Introduction** (5 pages) - Motivation, problem statement, contributions
2. **Background** (8 pages) - Actor model, Erlang/OTP, effect handlers
3. **System Design** (10 pages) - Architecture, process model, message passing
4. **Implementation** (12 pages) - Effect handlers, scheduler, memory management
5. **Evaluation** (10 pages) - Benchmarks, case studies, comparisons
6. **Related Work** (4 pages) - Actor systems, effect handlers, alternatives
7. **Conclusion** (2 pages) - Summary and future work
8. **Appendices** (3 pages) - API reference, installation, tuning

## 🎯 Key Message

All documents emphasize that **OCaml 5's effect handlers are the game-changing enabler** that makes elegant actor-model concurrency possible in a mainstream functional language.

## 🔨 Building Documents

```bash
# Build ICFP 2-page paper
make icfp

# Build full thesis
pdflatex thesis.tex
bibtex thesis
pdflatex thesis.tex
pdflatex thesis.tex

# Build all versions
make all
```

## 📚 Use Cases

- **ICFP Submission**: Use `icfp-2page.pdf`
- **Master's Thesis Defense**: Use `thesis.pdf`
- **Industry Presentation**: Use `paper-thesis.pdf`
- **Blog Post**: Extract sections from thesis

## ✍️ Author

**Leandro Ostera**
- Email: leandro@abstractmachines.dev
- Institution: Abstract Machines, Stockholm, Sweden

## 🚀 Future Work

The thesis outlines ambitious future directions:
- Distributed RIOT across data centers
- Formal verification of scheduler properties
- WebAssembly compilation for browser actors
- GPU-accelerated actors for ML workloads

## 📖 Citation

```bibtex
@mastersthesis{ostera2024riot,
  author = {Leandro Ostera},
  title = {RIOT: Unleashing Multi-core OCaml with Erlang-style Parallelism},
  school = {Abstract Machines},
  year = {2024},
  address = {Stockholm, Sweden}
}
```
