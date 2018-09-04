# !/bin/bash
# Author: Alexander Flynn-Carroll.af2017@imperial.ac.uk
# Script : CompileLaTeX.sh
# Desc: Creates a pdf doc from a .tex file and a bibliography
# Arguments: 16
# Date: 08 Feb 2018




# Build PDF

pdflatex miniproject.tex
pdflatex miniproject.tex
bibtex miniproject
pdflatex miniproject.tex
pdflatex miniproject.tex


# # Cleanup

[ -e *~ ] && rm *∼
[ -e *.aux ] && rm *.aux
[ -e *.dvi ] && rm *.dvi
[ -e *.log ] && rm *.log
[ -e *.nav ] && rm *.nav
[ -e *.out ] && rm *.out
[ -e *.snm ] && rm *.snm
[ -e *.toc ] && rm *.toc
[ -e *.blg ] && rm *.blg
[ -e *.bbl ] && rm *.bbl


