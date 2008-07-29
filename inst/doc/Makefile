#  $Id$
#
#  This is the makefile for the Waterfall documentation

all: pdf

clean: 
	$ rm -rf *.pdf *.ps *.dvi *.aux *.bbl *.log *.blg *.tex *-blx.bib *.eps

display: waterfall.pdf 
	$ xpdf waterfall.pdf

pdf waterfall.pdf: waterfall.ps
	$ ps2pdf waterfall.ps

postscript ps waterfall.ps: waterfall.dvi
	$ dvips -o waterfall.ps waterfall.dvi

dvi waterfall.dvi: waterfall.aux waterfall.bbl
	$ latex waterfall.tex && latex waterfall.tex

waterfall.aux: waterfall.tex
	$ latex waterfall.tex

bibtex waterfall.bbl: waterfall.bib
	$ bibtex waterfall

waterfall.tex: waterfall.Rnw
	$ R CMD Sweave waterfall.Rnw
