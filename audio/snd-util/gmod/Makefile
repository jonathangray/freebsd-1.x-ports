CFLAGS = -O2 -m486 -pipe

OFILES = cvt_period.o dump.o effects.o gmod.o init.o load_669.o load_mod.o \
	load_mtm.o load_s3m.o load_ult.o misc.o parse.o play_mod.o play_note.o \
	play_voice.o

STDHFILES = defines.h structs.h globals.h

.c.o:
	$(CC) $(CFLAGS) -c $<

all: gmod

gmod: $(OFILES)
	$(CC) -o gmod $(OFILES)
	strip gmod

clean:
	rm -f gmod $(OFILES)

cvt_period.o: cvt_period.c $(STDHFILES)

dump.o: dump.c $(STDHFILES)

effects.o: effects.c $(STDHFILES) protos.h

gmod.o: gmod.c defines.h structs.h tables.h protos.h

init.o: init.c $(STDHFILES)

load_669.o: load_669.c $(STDHFILES)

load_mod.o: load_mod.c $(STDHFILES) protos.h

load_mtm.o: load_mtm.c $(STDHFILES) protos.h

load_s3m.o: load_s3m.c

load_ult.o: load_ult.c $(STDHFILES) protos.h

misc.o: misc.c $(STDHFILES)

parse.o: parse.c $(STDHFILES)

play_mod.o: play_mod.c $(STDHFILES) protos.h

play_note.o: play_note.c $(STDHFILES) protos.h

play_voice.o: play_voice.c $(STDHFILES) protos.h

