# These are assigned by the recursive make.
#norm = unr
#karl = cunm

# Set on the cmdline for make fixup.
#weight=

# This is the order in which the fonts will be used.
symbol_sets = 8u 7j 10u 2n 9t 6j
norm_fonts = $(addprefix $(norm), $(symbol_sets))
tfm = $(addsuffix .tfm, $(norm_fonts))
mtx = $(addsuffix .mtx, $(norm_fonts))

dvilj = ../../dvilj
ECHO = /usr/local/gnu/bin/echo -e

default: all

# Also: hwir.tfm msyr.tfm (no vf's needed).
map = cork.map # Choices: plain.map cork.map test.map
all: $(map) k2n.awk
	for karl in `gawk '{print $$1}' $(map)`; do	\
	  norm=`gawk -f k2n.awk karl=$$karl $(map)`;		\
	  $(MAKE) --no-print-dir norm=$$norm karl=$$karl $$karl.lj; done

$(karl).lj: $(karl).dvi $(dvilj)
	$(dvilj) -w $< -p1 || rm -f $@

$(karl).dvi: $(karl).tfm $(karl).vf $(tfm)
	echo $(karl) | tex sample
	dvicopy sample.dvi $@ </dev/null || (rm -f $@ && exit 1)

$(karl).vf $(karl).tfm: $(karl)uq.vpl
	vptovf $< $(karl).vf $(karl).tfm

# uq for unique.
$(karl)uq.vpl: $(karl).vpl
	gawk -f rmligdups.awk $< >$@

$(karl).vpl: $(karl).top hptex.etx latin.mtx $(mtx) fontdim.tex \
             fontinst.rc fonthp.tex
	tex '\nonstopmode \input $<'

$(karl).top: $(norm).avail
	cp /dev/null $@
	$(ECHO) '\\def\\karl{$(karl)}' >>$@
	if echo $(karl) | grep '...[a-z0-9]*q' >/dev/null; then \
	  $(ECHO) '\\def\\outputetx{T1}' >>$@; \
	  else $(ECHO) '\\def\\outputetx{hptex}' >>$@; fi
	$(ECHO) '\\input $(norm).avail' >>$@
	$(ECHO) '\\input fonthp.tex' >>$@

$(norm).avail: $(tfm)
	cp /dev/null $@
	$(ECHO) -n '\\def\\avail{' >>$@
	-for f in $(tfm); do test -s $$f && printf `basename $$f .tfm`, >>$@; done
	$(ECHO) "}" >>$@

# This is bizarre, but it's fontinst that makes *.mtx.
# Only 10u and 9t really depend on hpsym{hi,lo}.etw, but oh well.
$(norm)%.mtx: $(norm)%.pl %.etx fontinst.rc ascii.etw uclc.tex \
	      etxmac.tex hpsymhi.etw hpsymlo.etw
	if test -s $<; then rm -f $@; else touch $@; fi

# If the font is lacking a certain symset, don't quit. We'll have an
# empty file in that case.
%.pl: %.tfm
	if test -s $<; then tftopl $< $@; else touch $@; fi

# Not all fonts have all the symbol sets we want.
# unzip exits 0 if the requested file is not in the archive. Create an
# empty file in that case, so make doesn't need to remake it.
$(norm)%.tfm:
	unzip ../../norm/zip/lj4tf$(norm).zip $@
	test -r $@ || touch $@

norm_dir = ../base
dump_dir = ../../karl/dump
main_dir = ..
install:
	mkdir -p $(norm_dir) $(dump_dir)
	for e in $(symbol_sets); do mv *$$e.tfm $(norm_dir); done
	for e in $(symbol_sets); do mv *$$e.mtx $(dump_dir); done
	for ext in avail top vpl; do mv *.$$ext $(dump_dir); done
	mv *.vf $(main_dir)

clean:
	mv latin.mtx save
	rm -f *.pl *.mtx *.vpl *.lj *.dvi *.log *.avail *.top
	mv save latin.mtx


ftm = $(addsuffix .ftm, $(norm_fonts))
fixup: $(ftm)
	for f in $(norm_fonts); do test -s $$f.ftm && mv $$f.ftm $$f.tfm; done

%.ftm: %.pl
	sed -e 's/^\((HEADER D 19 O .*\).)/\1$(weight))/' $< >temp.ftm
	pltotf temp.ftm $@
	rm -f temp.ftm
