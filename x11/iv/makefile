CPU=FREEBSD

all: src/Makefile
	@$(MAKE) $(.MAKEFLAGS) -f Makefile 'CPU=$(CPU)'

# simple but brutal. Will not install to multiple destinations simultaneously.
install:
	@rm -rf installed ${DESTDIR}/usr/local/interviews
	@mkdir -p ${DESTDIR}/usr/local/interviews
	@ln -s ${DESTDIR}/usr/local/interviews installed
	@$(MAKE) $(.MAKEFLAGS) -f Makefile 'CPU=$(CPU)' $@
package: all install
	pkg_create -c pkg/+ONELINE -d pkg/+DESC -f pkg/+PLIST iv_bin

src/Makefile:
	@$(MAKE) Makefiles

.DEFAULT: src/Makefile
	@$(MAKE) $(.MAKEFLAGS) -f Makefile 'CPU=$(CPU)' $@
