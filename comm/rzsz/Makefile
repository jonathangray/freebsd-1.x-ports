# Makefile for Unix/Xenix rz and sz programs
# the makefile is not too well tested yet
CC=gcc
CFLAGS+=-O
LBIN_DIR=/usr/local/bin
LCAT_DIR=/usr/local/man/cat1


ARCFILES= README rbsb.c gz *.t minirb.c zmodem.h \
	zm.c zmr.c crctab.c sz.c rz.c mailer.rz crc.c

all:posix crc
	@echo done

usenet:doc
	shar -c -a -n rzsz -o /tmp/rzsz -l64 \
	  README Makefile zmodem.h zm.c rz.c rbsb.c \
	 crc.c crctab.c minirb.c mailer.rz zmr.c *.doc gz sz.c *.t 

shar:doc
	shar -c README Makefile zmodem.h zm.c \
	 zmr.c sz.c rz.c crctab.c \
	 mailer.rz crc.c rbsb.c minirb.c *.doc gz *.t >/tmp/rzsz 

unixforum: shar
	compress /tmp/rzsz.sh

unix:
	undos $(ARCFILES)

dos:
	todos $(ARCFILES)

doc:rz.doc sz.doc crc.doc minirb.doc

minirb.doc:minirb.1
	nroff -man minirb.1 | col  >minirb.doc

rz.doc:rz.1
	nroff -man rz.1 | col  >rz.doc

sz.doc:sz.1
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
	 zmr.c crc.c *.1 *.doc gz *.t minirb.c

tags:
	ctags sz.c rz.c zm.c zmr.c rbsb.c

.PRECIOUS:rz sz

posix:
	$(CC) $(CFLAGS) -DPOSIX -DMD=2 rz.c -o rz
	size rz
	-rm -f rb rx rc
	ln rz rb
	ln rz rx
	ln rz rc
	$(CC) $(CFLAGS) -DPOSIX sz.c -o sz
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

sz: nothing
sb: nothing
rz: nothing
rb: nothing

crc:
	$(CC) crc.c -o crc


install:
	install -cs -o bin -m 755 rz rb rx rc $(LBIN_DIR)
	install -cs -o bin -m 755 sz sb sx zcommand zcommandi $(LBIN_DIR)
	install -cs -o bin -m 755 crc $(LBIN_DIR)
	install -cs -o bin -m 755 crc.doc minirb.doc rz.doc sz.doc $(LCAT_DIR)

clean:
	rm -f core rz rb rx rc sz sb sx zcommand zcommandi crc
