name = rapport

all: $(name).pdf

$(name).pdf: $(name).tex
	pdflatex $<

.PHONY: clean

clean:
	rm -f $(name).{aux,fdb_latexmk,fls,log,pdf,synctex.gz,toc}
