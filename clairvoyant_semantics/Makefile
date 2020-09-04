PDFLATEX = pdflatex
LATEX = latex

default: pdf

main:
	$(PDFLATEX) main
	bibtex main
	$(PDFLATEX) main
	$(PDFLATEX) main


bib: 
	$(PDFLATEX) main
	bibtex main
	$(PDFLATEX) main
	$(PDFLATEX) main
dvi: 
	$(LATEX) main

ps: dvi
	dvips -o main.ps -t letter main

pstopdf: ps
	ps2pdfwr main.ps main.pdf
pdf: 
	$(PDFLATEX)  main

final:
	rm -f main.pdf main.aux main.dvi main.log main.bbl main.blg
	$(PDFLATEX)  main
	bibtex main
	$(PDFLATEX)  main
	$(PDFLATEX)  main
	open main.pdf

clean: 
	rm main.aux main.dvi main.bbl main.blg main.log
