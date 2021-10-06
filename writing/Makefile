texb := pdflatex -interaction=nonstopmode -halt-on-error -file-line-error
appdoc = appendix
maindoc = main

.PHONY: clean prepare open
all: prepare $(appdoc).pdf $(maindoc).pdf clean #open

prepare:
	mkdir -p tab img
	cp ../*/output/*.pdf img
	cp ../*/output/*.tex tab
	rm -f $(maindoc).pdf
	rm -f $(appdoc).pdf

$(maindoc).pdf: $(maindoc).tex $(appdoc).pdf
	$(texb) $<
	bibtex $(maindoc).aux
	$(texb) $<
	$(texb) $<

$(appdoc).pdf: $(appdoc).tex
	$(texb) $<
	# bibtex $(appdoc).aux
	$(texb) $<
	$(texb) $<

clean:
	find . | egrep '\.(aux|log|toc|blg|lot|lof|bbl|out|DS|ent|fdb|fls|gz)' | xargs rm -f

# open:
# 	open $(maindoc).pdf -a "Skim"
