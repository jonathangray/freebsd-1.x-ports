CPU=FREEBSD

all: src/Makefile
	@$(MAKE) $(.MAKEFLAGS) -f Makefile 'CPU=$(CPU)'

# simple but brutal. Will not install to multiple destinations simultaneously.
install: Makefile
	@rm -rf installed ${DESTDIR}/usr/local/interviews
	@mkdir -p ${DESTDIR}/usr/local/interviews
	@ln -s ${DESTDIR}/usr/local/interviews installed
	@$(MAKE) $(.MAKEFLAGS) -f Makefile 'CPU=$(CPU)' $@
package: all install
	pkg_create -c pkg/+ONELINE -d pkg/+DESC -f pkg/+PLIST iv_bin

src/Makefile: Makefile
	@$(MAKE) Makefiles

Makefile:
	imake -T "InterViews/template" -I./src/config \
		-I/usr/X386/lib/X11/config -DTOPDIR=. -DCURDIR=.

.DEFAULT: src/Makefile
	@$(MAKE) $(.MAKEFLAGS) -f Makefile 'CPU=$(CPU)' $@
