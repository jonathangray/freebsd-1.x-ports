BINDIR=		/usr/local/bin
ETCDIR=		/usr/local/etc
EXECDIR=	/usr/local/libexec
MANDIR=		/usr/local/man
SAMPLE?=	.sample

all:
	sh ${.CURDIR}/build fbs

install: bin/ftpd bin/ftpcount bin/ftpshut bin/ftpwho
	@echo Installing binaries.
	install ${COPY} ${STRIP} -o bin -g bin -m 755 \
		bin/ftpd ${EXECDIR}/ftpd
	install ${COPY} ${STRIP} -o bin -g bin -m 755 \
		bin/ftpcount ${BINDIR}/ftpcount
	install ${COPY} ${STRIP} -o bin -g bin -m 755 \
		bin/ftpshut ${BINDIR}/ftpshut
	install ${COPY} ${STRIP} -o bin -g bin -m 755 \
		bin/ftpwho ${BINDIR}/ftpwho
	@echo Installing manpages.
	install ${COPY} -o bin -g bin -m 755 \
		doc/ftpcount.1 ${MANDIR}/man1/ftpcount.1
	install ${COPY} -o bin -g bin -m 755 \
		doc/ftpwho.1 ${MANDIR}/man1/ftpwho.1
	install ${COPY} -o bin -g bin -m 755 \
		doc/ftpaccess.5 ${MANDIR}/man5/ftpaccess.5
	install ${COPY} -o bin -g bin -m 755 \
		doc/ftphosts.5 ${MANDIR}/man5/ftphosts.5
	install ${COPY} -o bin -g bin -m 755 \
		doc/ftpconversions.5 ${MANDIR}/man5/ftpconversions.5
	install ${COPY} -o bin -g bin -m 755 \
		doc/xferlog.5 ${MANDIR}/man5/xferlog.5
	install ${COPY} -o bin -g bin -m 755 \
		doc/ftpd.8 ${MANDIR}/man8/ftpd.8
	install ${COPY} -o bin -g bin -m 755 \
		doc/ftpshut.8 ${MANDIR}/man8/ftpshut.8
	@echo Installing config files.
	install ${COPY} -o root -g wheel -m 644 \
		doc/examples/ftpaccess ${ETCDIR}/ftpaccess${SAMPLE}
	install ${COPY} -o root -g wheel -m 644 \
		doc/examples/ftpconversions ${ETCDIR}/ftpconversions${SAMPLE}
	install ${COPY} -o root -g wheel -m 644 \
		doc/examples/ftpgroups ${ETCDIR}/ftpgroups${SAMPLE}
	install ${COPY} -o root -g wheel -m 644 \
		doc/examples/ftphosts ${ETCDIR}/ftphosts${SAMPLE}

clean:
	sh ${.CURDIR}/build clean

package: install
	pkg_create \
		-c ${.CURDIR}/pkg/+ONELINE \
		-d ${.CURDIR}/pkg/+DESC \
		-f ${.CURDIR}/pkg/+PLIST \
		wu-ftpd_bin
