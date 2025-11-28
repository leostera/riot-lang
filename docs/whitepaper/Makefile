# Makefile for building the MiniRiot whitepaper

MAIN = paper
SOURCES = $(MAIN).tex references.bib
PDF = $(MAIN).pdf

# LaTeX compiler
BIN=/Library/TeX/Distributions/Programs/texbin/
LATEX = $(BIN)/pdflatex
BIBTEX = $(BIN)/bibtex

# Flags
LATEX_FLAGS = -interaction=nonstopmode -halt-on-error

.PHONY: all clean view

all: $(PDF)

$(PDF): $(SOURCES)
	$(LATEX) $(LATEX_FLAGS) $(MAIN)
	$(BIBTEX) $(MAIN) || true
	$(LATEX) $(LATEX_FLAGS) $(MAIN)
	$(LATEX) $(LATEX_FLAGS) $(MAIN)

clean:
	rm -f $(MAIN).aux $(MAIN).bbl $(MAIN).blg $(MAIN).log $(MAIN).out $(MAIN).toc $(MAIN).pdf
	rm -f *.aux *.log *.out *.bbl *.blg *.toc

view: $(PDF)
	open $(PDF) || xdg-open $(PDF) || echo "Please open $(PDF) manually"

# Watch for changes and rebuild
watch:
	while true; do \
		inotifywait -e modify $(SOURCES); \
		make all; \
	done

help:
	@echo "Available targets:"
	@echo "  all    - Build the PDF (default)"
	@echo "  clean  - Remove all generated files"
	@echo "  view   - Build and open the PDF"
	@echo "  watch  - Continuously rebuild on file changes"
	@echo "  help   - Show this help message"

# Additional targets for ICFP and thesis
icfp: icfp-2page.pdf

thesis: thesis.pdf

icfp-2page.pdf: icfp-2page.tex references.bib
	$(LATEX) $(LATEX_FLAGS) icfp-2page
	$(BIBTEX) icfp-2page || true
	$(LATEX) $(LATEX_FLAGS) icfp-2page
	$(LATEX) $(LATEX_FLAGS) icfp-2page

thesis.pdf: thesis.tex references.bib
	$(LATEX) $(LATEX_FLAGS) thesis
	$(BIBTEX) thesis || true
	$(LATEX) $(LATEX_FLAGS) thesis
	$(LATEX) $(LATEX_FLAGS) thesis

all-papers: $(PDF) icfp-2page.pdf thesis.pdf

clean-all: clean
	rm -f icfp-2page.* thesis.*
