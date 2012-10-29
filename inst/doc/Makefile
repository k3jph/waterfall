#
#
#

LATEX	:= xelatex
SWEAVE	:= R CMD Sweave
BIBTEX	:= bibtex
MKIDX	:= makeindex
MKGLS	:= makeglossaries

all: waterfall.pdf

waterfall.pdf: waterfall.bcf waterfall.tex waterfall.bbl
	$ $(LATEX) waterfall
	$ $(LATEX) waterfall

waterfall.bbl: waterfall.tex Makefile
	$ $(BIBTEX) waterfall

waterfall.bcf: waterfall.tex Makefile
	$ $(LATEX) waterfall

waterfall.tex: waterfall.Rnw Makefile
	$ $(SWEAVE) waterfall

clean:
	$ hg purge
