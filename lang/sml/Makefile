PROGS = src/sml src/smld cml-0.9.8/cml eXene-0.4/eXene

all: $(PROGS)

src/sml:
	(cd src; ./makeml -i386 FreeBSD)

src/smld:
	(cd src; ./makeml -i386 FreeBSD -debug -i)

cml-0.9.8/cml: src/sml
	(cd cml-0.9.8; ./install-cml -sml ../src/sml)

eXene-0.4/eXene: cml-0.9.8/cml
	(cd eXene-0.4; ./install -cml ../cml-0.9.8/cml -lib ../smlnj-lib-0.2)

clean:
	rm -f $(PROGS)
	(cd src/runtime; make clean)

install: $(PROGS)
	-mkdir -p /usr/local/bin
	install -s $(PROGS) /usr/local/bin
	-mkdir -p /usr/local/lib/sml/smlnj-lib
	(cd smlnj-lib-0.2; \
	    install -c `echo * | sed -e s/CVS//` /usr/local/lib/sml/smlnj-lib)
	-mkdir -p /usr/local/man/man1
	install -c doc/man1/sml.1 /usr/local/man/man1
	-mkdir -p /usr/gnu/lib/emacs/site-lisp
	install -c -m 444 tools/debug/*.el /usr/gnu/lib/emacs/site-lisp

package: pkg_bin

pkg_bin: install
	pkg_create -c pkg/+ONELINE -d doc/release-notes/release-notes.txt -f pkg/+PLIST sml93_bin
