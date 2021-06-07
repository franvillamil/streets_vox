texb := pdflatex -interaction=nonstopmode -halt-on-error -file-line-error
appdoc = appendix
maindoc = main

.PHONY: clean subdir open
all: subdir $(appdoc).pdf $(maindoc).pdf clean open

subdir:
	mkdir -p tab img
	cp ../*/output/*.pdf img
	cp ../*/output/*.tex tab

$(maindoc).pdf: $(maindoc).tex $(appdoc).tex
	$(texb) $<
	bibtex $(maindoc).aux
	$(texb) $<
	$(texb) $<
	open $<

$(appdoc).pdf: $(appdoc).tex
	$(texb) $<
	bibtex $(appdoc).aux
	$(texb) $<
	$(texb) $<

clean:
	find . | egrep '\.(aux|log|toc|blg|bbl|out|DS|ent|fdb|fls|gz)' | xargs rm -f

open:
	open $(maindoc).pdf -a "Skim"
