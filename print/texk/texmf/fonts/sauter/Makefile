# Makefile for any point-size Computer Modern, LaTeX symbol, Computer
# Modern Cyrillic.  Since it's not critical that this be portable, I
# used pattern rules and other features not provided by all makes.
# I recommend GNU make.

version = 2.0

MF = cmmf

default:
	@echo "Please specify an explicit target."
	@echo "(Are you sure you want to be using this Makefile?)"

# For testing a single font.
fam = cmbsy
size = 5
font = $(fam)$(size)

.PRECIOUS: %.tfm %gf

# We use the script to get the correct magic sizes for 17, 20, etc.
mf/%.mf:
	./make-mf $(fam) $(size)

orig/%.tfm: b-$(fam).mf
	cd orig; $(MF) '\mode:=localfont; nonstopmode; input '$*

%.tfm %.300gf: mf/%.mf b-$(fam).mf
	$(MF) '\mode:=localfont; nonstopmode; input '$<

%.pl: %.tfm
	tftopl $< $@

# Need GNU diff for this.
diffopts = --show-function='^(\(CHAR\|LABEL\)' -u0
compare: $(font).pl orig/$(font).pl
	diff $(diffopts) orig/$(font).pl $(font).pl | tee /tmp/diff

# No, I didn't try to write down the complete dependencies for all of
# Computer Modern, just what I needed as I was doing the testing.
cm_dir = /usr/local/src/TeX+MF/typefaces/cm

cmbsy5.tfm cmbsy6.tfm cmbsy7.tfm cmbsy8.tfm cmbsy9.tfm cmbsy10.tfm: \
  c-sigma.mf c-bmath.mf
cmbsy5.tfm orig/cmbsy5.tfm: $(cm_dir)/symbol.mf $(cm_dir)/sym.mf
cmcsc8.tfm orig/cmcsc8.tfm: $(cm_dir)/csc.mf $(cm_dir)/cscspu.mf
cmex7.tfm orig/cmex7.tfm: $(cm_dir)/mathex.mf
cmmib5.tfm orig/cmmib5.tfm: $(cm_dir)/greeku.mf $(cm_dir)/greekl.mf \
  $(cm_dir)/itall.mf


check:
	$(MAKE) check-family fam=cmb sizes=10
	$(MAKE) check-family fam=cmbsy sizes="5 6 7 8 9 10"
	$(MAKE) check-family fam=cmbx sizes="5 6 7 8 9 10 12"
	$(MAKE) check-family fam=cmbxsl sizes=10
	$(MAKE) check-family fam=cmbxti sizes=10
	$(MAKE) check-family fam=cmcsc sizes="8 9 10"
	$(MAKE) check-family fam=cmdunh sizes=10
	$(MAKE) check-family fam=cmex sizes="7 8 9 10"
	$(MAKE) check-family fam=cmff sizes=10
	$(MAKE) check-family fam=cmfi sizes=10
	$(MAKE) check-family fam=cmfib sizes=8
	$(MAKE) check-family fam=cmitt sizes=10
	$(MAKE) check-family fam=cmmi sizes="5 6 7 8 9 10 12"
	$(MAKE) check-family fam=cmmib sizes="5 6 7 8 9 10"
	$(MAKE) check-family fam=cmr sizes="5 6 7 8 9 10 12 17"
	$(MAKE) check-family fam=cmsl sizes="8 10 12"
	$(MAKE) check-family fam=cmsltt sizes=10
	$(MAKE) check-family fam=cmss sizes="8 9 10 12 17"
	$(MAKE) check-family fam=cmssbx sizes=10
	$(MAKE) check-family fam=cmssdc sizes=10
	$(MAKE) check-family fam=cmssi sizes="8 9 10 12 17"
	$(MAKE) check-family fam=cmssq sizes=8
	$(MAKE) check-family fam=cmssqi sizes=8
	$(MAKE) check-family fam=cmsy sizes="5 6 7 8 9 9 10"
	$(MAKE) check-family fam=cmtcsc sizes=10
	$(MAKE) check-family fam=cmtex sizes="8 9 10"
	$(MAKE) check-family fam=cmti sizes="7 8 9 10 12"
	$(MAKE) check-family fam=cmtt sizes="8 9 10 12"
	$(MAKE) check-family fam=cmu sizes=10
	$(MAKE) check-family fam=cmvtt sizes=10

check-family:
	for s in $(sizes); do $(MAKE) size=$$s compare; done

# Do this one at a time, to avoid overflowing the arg limit.
mostlyclean clean:
	rm -f core
	rm -f *.log
	rm -f *.*gf
	rm -f *.pl
	rm -f *.ok
	rm -f *.tfm

distclean realclean: clean
	rm -f make_gf_files make_mf_files make_pk_files mf/* gf/* pk/* tfm/*

distdir = sauter-$(version)
dist:
	rm -rf $(distdir)
	mkdir $(distdir)
	ln C* M* R* b*.mf c*.mf try*.in $(distdir)
	cp /usr/local/bin/MakeTeXPK $(distdir)
	cp /usr/local/lib/tex/macros/latex/lfonts.true $(distdir)
	ln make-cyr make-latex-mf make-mf makemake-pk $(distdir)
	ln fontdef.sau $(distdir)
	cd $(distdir); mkdir gf mf pk tfm
	tar czf $(distdir). $(distdir)
	rm -rf $(distdir)
