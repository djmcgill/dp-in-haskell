# Makefile for the PATAT 2014

.PHONY: all ps pdf bib clean

.SUFFIXES: .tex .lhs

.lhs.tex:
	lhs2TeX --poly -o $@ $<

TARGET = extended-abstract

TEXSOURCES = \
    extended-abstract.tex \
    abstract.tex \
    introduction.tex \
    results.tex \
    abstraction.tex

LHSSOURCES = \
#    eurosim2010.lhs \
#    fhm.lhs \
#    fullwave.lhs \
#    halfwave.lhs

GENTEXSOURCES = $(LHSSOURCES:.lhs=.tex)

ALLSOURCES = \
    ${TEXSOURCES} \
    ${GENTEXSOURCES} \
    cites.bib

all: pdf

pdf: ${TARGET}.pdf

${TARGET}.pdf: ${ALLSOURCES}
	pdflatex ${TARGET}
	bibtex ${TARGET}
	pdflatex ${TARGET}
	pdflatex ${TARGET}

# ${GENTEXSOURCES}: lhs2tex-arrows-fhm-preamble

bib:
	rm -rf *.bbl *.blg
	bibtex ${TARGET}

# Diagrams: FORCE
#	cd $@ && $(MAKE) all

clean:
	rm -rf ${TARGET}.pdf \
           ${GENTEXSOURCES} \
           *.aux *.dvi *.inx *.log *.bbl *.blg *.ptb *~
#	cd Diagrams && $(MAKE) clean

FORCE:
