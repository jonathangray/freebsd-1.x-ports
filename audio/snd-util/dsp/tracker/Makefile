O = .o
UI = Unix/./
BIN_DIR = /usr/local/bin
MAN_DIR = /usr/local/man
INSTALL = install
INST_BIN_OPT = -s -m 755 -o bin -g bin
INST_MAN_OPT = -m 644 -o bin -g bin

CFLAGS = -O2 -funroll-loops
LIBS = -lm
AUDIO = Linux/./
CONFIG = Linux/

COPTS = $(OPTS) $(CFLAGS)

OBJ_TRACKER = main$O $(AUDIO)audio$O st_read$O commands$O \
audio$O automaton$O player$O getopt$O open$O tools$O \
dump_song$O setup_audio$O notes$O display$O \
$(UI)ui$O prefs$O tags$O autoinit$O

OBJ_ANALYZER = analyzer$O st_read$O open$O dump_song$O tools$O notes$O \
prefs$O autoinit$O ${UI}ui$O display$O

OBJ_SPLITMOD = split$O tools$O st_read$O dump_song$O open$O notes$O \
display$O prefs$O autoinit$O $(UI)ui$O


all:	config.h tracker randomize analyzer splitmod

config.h:
	cp $(CONFIG)config.h config.h

install: 
	$(INSTALL) $(INST_BIN_OPT) tracker $(BIN_DIR)
	$(INSTALL) $(INST_BIN_OPT) randomize $(BIN_DIR)
	$(INSTALL) $(INST_BIN_OPT) analyzer $(BIN_DIR)
	$(INSTALL) $(INST_BIN_OPT) splitmod $(BIN_DIR)
	$(INSTALL) $(INST_MAN_OPT) man/tracker.1 $(MAN_DIR)/man1/tracker.1

tracker: ${OBJ_TRACKER}
	${CC} -o tracker ${CFLAGS} ${OBJ_TRACKER} ${LIBS}

analyzer: ${OBJ_ANALYZER}
	$(CC) -o analyzer ${CFLAGS} ${OBJ_ANALYZER} ${LIBS}

randomize: randomize.c
	$(CC) -o randomize ${CFLAGS} randomize.c 

splitmod: ${OBJ_SPLITMOD}
	$(CC) -o splitmod ${CFLAGS} ${OBJ_SPLITMOD} ${LIBS}

main$O: main.c song.h
	$(CC) ${COPTS} main.c

tools$O: main.c 
	$(CC) ${COPTS} tools.c

$(AUDIO)audio$O: $(AUDIO)audio.c 
	cd `dirname $(AUDIO)`; $(CC) -I.. ${COPTS} audio.c

$(UI)ui$O: $(UI)ui.c
	cd $(UI); $(CC) -I.. ${COPTS} ui.c

split$O: split.c
	$(CC) ${COPTS} split.c

open$O: open.c
	$(CC) ${COPTS} open.c

audio$O: audio.c
	$(CC) ${COPTS} audio.c

automaton$O: automaton.c song.h channel.h
	$(CC) ${COPTS} automaton.c

getopt$O: getopt.c getopt.h
	$(CC) ${COPTS} getopt.c

getopt1$O: getopt1.c getopt.h
	$(CC) ${COPTS} getopt1.c

player$O: player.c song.h channel.h
	$(CC) ${COPTS} player.c

st_read$O: st_read.c song.h
	$(CC) ${COPTS} st_read.c

commands$O: commands.c channel.h song.h
	$(CC) ${COPTS} commands.c

dump_song$O: dump_song.c
	$(CC) $(COPTS) dump_song.c
analyzer$O: analyzer.c
	$(CC) $(COPTS) analyzer.c
setup_audio$O: setup_audio.c
	$(CC) $(COPTS) setup_audio.c
notes$O: notes.c
	$(CC) $(COPTS) notes.c

display$O: display.c
	$(CC) $(COPTS) display.c


clean:
	-rm -f config.h $(OBJ_TRACKER) $(OBJ_ANALYZER) $(OBJ_SPLITMOD) randomize$O
	-rm -f tracker randomize splitmod analyzer core*

