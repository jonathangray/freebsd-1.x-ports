# ingres 8.9 Makefile

PREFIX=/usr/local
INGRESROOT=`grep ingres /etc/passwd | cut -d : -f 6`

DESTBIN=${PREFIX}/bin
DESTLIB=${PREFIX}/lib
DESTSVR=${PREFIX}/etc
INGRESBIN=${INGRESROOT}/bin
INGRESLIB=${INGRESROOT}/lib

PROGS=ingres sysmod creatdb destroydb demodb purge restore copydb \
    printr helpr ingconv equel

all: ingres
	@echo done

ingres:
	PWD=`pwd`; \
	INGRES_HOME=`grep ingres /etc/passwd | cut -d : -f 6`; \
	cd $$INGRES_HOME; \
	INGRES_HOME=`pwd`; \
	if [ ! "$${INGRES_HOME}" = "$${PWD}" ]; then \
		echo "Before compiling, ingres must have a login,"; \
		echo "the source must be in ingres's login directory,"; \
		echo "and compiling must be done  by user ingres"; \
		echo "using GCC version 1.x"; \
		echo "See the README files for more information."; \
		exit 1; \
	fi; \
	if whoami | grep ingres; then : ; \
	else \
		echo "compile as user ingres"; \
		exit 1; \
	fi; \
	cd src/conf; \
	make INGRES=$$INGRES_HOME allinstall

install:
	@if whoami | egrep 'root|bin'; then : ; \
	else \
		echo "install as either user root or bin"; \
		exit 1; \
	fi
	@if [ ! -f ${INGRESBIN}/ingres ]; then \
		echo "must compile source first as user ingres"; exit 1; \
	fi
	-mkdir -p ${DESTBIN}
	@cd ${DESTBIN}; \
	for i in ${PROGS}; do \
		rm -f $$i; \
		ln -s ${INGRESBIN}/$$i; \
	done
	-mkdir -p ${DESTSVR}
	cd ${DESTSVR}; \
	rm -f ingreslock; \
	ln -s ${INGRESBIN}/ingreslock
	-mkdir -p ${DESTLIB}
	cd ${DESTLIB}; \
	rm -f libq.a; \
	ln -s ${INGRESLIB}/libq.a
	@if grep -s ingreslock /etc/rc.local; then : ; else \
		echo; \
		echo =================================================================; \
		echo "Still need to modify /etc/rc.local to invoke ingreslock on boot."; \
		echo "Add a line like:";  \
		echo "${DESTSVR}/ingreslock & echo -n ' ingreslock' > /var/log/ingreslock.log"; \
		echo; \
		echo "Add to /etc/services a line like:"; \
		echo "ingreslock     1524/tcp       # Ingres concurrency driver"; \
		echo =================================================================; \
	fi

clean:
	cd src/conf; \
	make allclean

depend:
	cd src/conf; \
	make alldepend
