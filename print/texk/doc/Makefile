SUBDIR= ams/amsfonts ams/amslatex ams/amstex amslatex-primer bibtex \
	components-of-TeX dvips eplain fontname-1.6 \
	gentle-introduction kpathsea latex-primer \
	latex-help-texinfo makeindex makeindex-paper \
	metafont-primer supertab texdraw tugboat \
	web-manual whatstex

# errata help latex-manual patgen metafont-book tex-book 

all:
	for entry in $(SUBDIR); do  \
		(echo "===> $$entry";  cd $$entry;  make all); \
	done

clean realclean:
	for entry in $(SUBDIR); do  \
		(echo "===> $$entry";  cd $$entry;  make $@); \
	done
	
