# Makefile for Unix/Xenix rz and sz programs
# the makefile is not too well tested yet
CC=cc
OFLAG= -O


ARCFILES= README rbsb.c gz *.t minirb.c zmodem.h \
	zm.c zmr.c crctab.c sz.c rz.c mailer.rz crc.c

nothing:
	@echo
	@echo "Please study the #ifdef's in crctab.c, rbsb.c, rz.c and sz.c,"
	@echo "make any necessary hacks for oddball or merged SYSV/BSD systems,"
	@echo "then type 'make SYSTEM' where SYSTEM is one of:"
	@echo
	@echo "	posix	POSIX compliant systems"
	@echo "	odt	SCO Open Desktop"
	@echo "	icc	SCO Open Desktop, Intel compiler"
	@echo "	sysvr4	SYSTEM 5.4 Unix, SCO Open Desktop"
	@echo "	sysvr3	SYSTEM 5.3 Unix with mkdir(2), COHERENT 4.2"
	@echo "	sysv	SYSTEM 3/5 Unix"
	@echo "	xenix	Xenix"
	@echo "	x386	386 Xenix"
	@echo "	bsd	Berkeley 4.x BSD, Ultrix, V7"
	@echo "	tandy	Tandy 6000 Xenix"
	@echo "	dnix	DIAB Dnix 5.2"
	@echo "	dnix5r3	DIAB Dnix 5.3"
	@echo "	amiga	3000UX running SVR4"
	@echo "	POSIX	POSIX compliant systems (SCO Open Desktop, strict)"
	@echo
	@echo "	doc	Format the man pages with nroff"
	@echo

usenet:doc
	shar -c -a -n rzsz -o /tmp/rzsz -l64 \
	  README Makefile zmodem.h zm.c rz.c rbsb.c \
	 crc.c crctab.c minirb.c mailer.rz zmr.c *.doc gz sz.c *.t 

sshar:doc
	shar -c -a -n rzsz -o /tmp/rzsz -l64 \
	  README Makefile zmodem.h zm.c rz.c rbsb.c \
	 crc.c crctab.c mailer.rz zmr.c *.1 gz sz.c

shar:doc
	shar -c README Makefile zmodem.h zm.c \
	 zmr.c sz.c rz.c crctab.c \
	 mailer.rz crc.c rbsb.c minirb.c *.doc gz *.t >/tmp/rzsz.sh
	 cp /tmp/rzsz.sh /u/t/yam

unixforum: shar
	rm -f /tmp/rzsz.sh.Z
	compress /tmp/rzsz.sh
	cp /tmp/rzsz.sh.Z /u/t/yam

unix:
	undos $(ARCFILES)

dos:
	todos $(ARCFILES)

doc:rz.doc sz.doc crc.doc minirb.doc

clean:
	rm -f *.o *.out sz sb sx zcommand zcommandi rz rb rx rc

minirb.doc:minirb.1
	nroff -man minirb.1 | col  >minirb.doc

rz.doc:rz.1 servers.mi
	nroff -man rz.1 | col  >rz.doc

sz.doc:sz.1 servers.mi
	nroff -man sz.1 | col  >sz.doc

crc.doc:crc.1
	nroff -man crc.1 | col  >crc.doc

zoo: doc
	-rm -f /tmp/rzsz.zoo
	zoo ah /tmp/rzsz README Makefile zmodem.h zm.c sz.c rz.c \
	 mailer.rz crctab.c rbsb.c *.doc \
	 zmr.c crc.c gz *.t minirb.c
	touch /tmp/rzsz.zoo
	chmod og-w /tmp/rzsz.zoo
	mv /tmp/rzsz.zoo /u/t/yam
	-rm -f rzsz.zip
	zip rzsz readme mailer.rz makefile zmodem.h zm.c sz.c rz.c
	zip rzsz crctab.c rbsb.c *.doc
	zip rzsz zmr.c crc.c gz *.t minirb.c
	mv rzsz.zip /u/t/yam

tar:doc
	tar cvf /tmp/rzsz.tar README Makefile zmodem.h zm.c sz.c rz.c \
	 mailer.rz crctab.c rbsb.c \
	 zmr.c crc.c *.1 gz *.t minirb.c

tags:
	ctags sz.c rz.c zm.c zmr.c rbsb.c

.PRECIOUS:rz sz

xenix:
	$(CC) $(CFLAGS) $(OFLAG) -M0 -K -i -DUSG -DNFGVMIN -DREADCHECK sz.c -lx -o sz
	size sz
	-rm -f sb sx zcommand zcommandi
	ln sz sb
	ln sz sx
	ln sz zcommand
	ln sz zcommandi
	$(CC) $(CFLAGS) $(OFLAG) -M0 -K -i -DUSG -DMD rz.c -o rz
	size rz
	-rm -f rb rx rc
	ln rz rb
	ln rz rx
	ln rz rc

x386:
	$(CC) $(CFLAGS) $(OFLAG) -DUSG -DMD rz.c -o rz
	size rz
	-rm -f rb rx rc
	ln rz rb
	ln rz rx
	ln rz rc
	$(CC) $(CFLAGS) $(OFLAG) -DUSG -DNFGVMIN -DREADCHECK sz.c -lx -o sz
	size sz
	-rm -f sb sx zcommand zcommandi
	ln sz sb
	ln sz sx
	ln sz zcommand
	ln sz zcommandi

sysv:
	$(CC) $(CFLAGS) $(OFLAG) -DUSG -DMD rz.c -o rz
	size rz
	-rm -f rb rx rc
	ln rz rb
	ln rz rx
	ln rz rc
	$(CC) $(CFLAGS) $(OFLAG) -DUSG -DSV -DNFGVMIN sz.c -o sz
	size sz
	-rm -f sb sx zcommand zcommandi
	ln sz sb
	ln sz sx
	ln sz zcommand
	ln sz zcommandi

sysvr3:
	$(CC) $(CFLAGS) $(OFLAG) -DUSG -DMD=2 rz.c -o rz
	size rz
	-rm -f rb rx rc
	ln rz rb
	ln rz rx
	ln rz rc
	$(CC) $(CFLAGS) $(OFLAG) -DSV -DUSG -DNFGVMIN sz.c -o sz
	size sz
	-rm -f sb sx zcommand zcommandi
	ln sz sb
	ln sz sx
	ln sz zcommand
	ln sz zcommandi

sysvr4:
	$(CC) $(CFLAGS) $(OFLAG) -DUSG -DMD=2 rz.c -o rz
	size rz
	-rm -f rb rx rc
	ln rz rb
	ln rz rx
	ln rz rc
	$(CC) $(CFLAGS) $(OFLAG) -DSV -DUSG sz.c -o sz
	size sz
	-rm -f sb sx zcommand zcommandi
	ln sz sb
	ln sz sx
	ln sz zcommand
	ln sz zcommandi

odt:
	cc -strict -W2 -n -DUSG -DMD=2 rz.c -o rz
	size rz
	-rm -f rb rx rc
	ln rz rb
	ln rz rx
	ln rz rc
	cc -strict -W2 -n -DUSG -DREADCHECK sz.c -lx -o sz
	size sz
	-rm -f sb sx zcommand zcommandi
	ln sz sb
	ln sz sx
	ln sz zcommand
	ln sz zcommandi

icc:
	icc -O -ip -mem -DUSG -DMD=2 rz.c -o rz
	size rz
	-rm -f rb rx rc
	ln rz rb
	ln rz rx
	ln rz rc
	icc -O -ip -mem -DUSG -DREADCHECK sz.c -lx -o sz
	size sz
	-rm -f sb sx zcommand zcommandi
	ln sz sb
	ln sz sx
	ln sz zcommand
	ln sz zcommandi

posix:
	$(CC) $(CFLAGS) $(OFLAG) -DPOSIX -DMD=2 rz.c -o rz
	size rz
	-rm -f rb rx rc
	ln rz rb
	ln rz rx
	ln rz rc
	$(CC) $(CFLAGS) $(OFLAG) -DPOSIX sz.c -o sz
	size sz
	-rm -f sb sx zcommand zcommandi
	ln sz sb
	ln sz sx
	ln sz zcommand
	ln sz zcommandi

POSIX:
	@echo "Well, stricter, as in *safer sex* ..."
	$(CC) $(CFLAGS) $(OFLAG) -posix -W2 -DPOSIX -DMD=2 rz.c -o rz
	size rz
	-rm -f rb rx rc
	ln rz rb
	ln rz rx
	ln rz rc
	$(CC) $(CFLAGS) $(OFLAG) -posix -W2 -DPOSIX sz.c -o sz
	size sz
	-rm -f sb sx zcommand zcommandi
	ln sz sb
	ln sz sx
	ln sz zcommand
	ln sz zcommandi


bsd:
	$(CC) $(CFLAGS) $(OFLAG) -DMD=2 -Dstrchr=index -DV7 rz.c -o rz
	size rz
	-rm -f rb rx rc
	ln rz rb
	ln rz rx
	ln rz rc
	$(CC) $(CFLAGS) $(OFLAG) -DV7 -DNFGVMIN sz.c -o sz
	size sz
	-rm -f sb sx zcommand zcommandi
	ln sz sb
	ln sz sx
	ln sz zcommand
	ln sz zcommandi

tandy:
	$(CC) $(CFLAGS) $(OFLAGS) -n -DUSG -DMD -DT6K sz.c -lx -o sz
	size sz
	-rm -f sb sx zcommand zcommandi
	ln sz sb
	ln sz sx
	ln sz zcommand
	ln sz zcommandi
	$(CC) $(CFLAGS) $(OFLAGS) -n -DUSG -DMD -DT6K rz.c -lx -o rz
	size rz
	-rm -f rb rx rc
	ln rz rb
	ln rz rx
	ln rz rc

dnix:
	$(CC) $(CFLAGS) $(OFLAG) -DUSG -DMD rz.c -o rz
	size rz
	-rm -f rb rx rc
	ln rz rb
	ln rz rx
	ln rz rc
	$(CC) $(CFLAGS) $(OFLAG) -DSV -DUSG -DNFGVMIN -DREADCHECK sz.c -o sz
	size sz
	-rm -f sb sx zcommand zcommandi
	ln sz sb
	ln sz sx
	ln sz zcommand
	ln sz zcommandi

dnix5r3:
	$(CC) $(CFLAGS) $(OFLAG) -DUSG -DMD=2 rz.c -o rz
	size rz
	-rm -f rb rx rc
	ln rz rb
	ln rz rx
	ln rz rc
	$(CC) $(CFLAGS) $(OFLAG) -DUSG -DSV -DNFGVMIN -DREADCHECK sz.c -o sz
	size sz
	-rm -f sb sx zcommand zcommandi
	ln sz sb
	ln sz sx
	ln sz zcommand
	ln sz zcommandi


amiga:
	$(CC) $(CFLAGS) $(OFLAG) -DUSG -DNFGVMIN -g rz.c -o rz
	size rz
	-rm -f rb rx rc
	ln rz rb
	ln rz rx
	ln rz rc
	$(CC) $(CFLAGS) $(OFLAG) -DUSG -DSV -DNFGVMIN -g sz.c -o sz
	size sz
	-rm -f sb sx zcommand zcommandi
	ln sz sb
	ln sz sx
	ln sz zcommand
	ln sz zcommandi



sz: nothing
sb: nothing
rz: nothing
rb: nothing
