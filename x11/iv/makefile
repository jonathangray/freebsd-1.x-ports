CPU=FREEBSD

all:
	@$(MAKE) $(.MAKEFLAGS) -f Makefile 'CPU=$(CPU)'

# simple but brutal
install:
	@rm -rf installed /usr/local/interviews
	@mkdir -p /usr/local/interviews
	@ln -s /usr/local/interviews installed
	@$(MAKE) $(.MAKEFLAGS) -f Makefile 'CPU=$(CPU)' $@
package: all install
	pkg_create -c pkg/+ONELINE -d pkg/+DESC -f pkg/+PLIST iv_bin

.DEFAULT:
	@$(MAKE) $(.MAKEFLAGS) -f Makefile 'CPU=$(CPU)' $@
