O = .o
OPTS = -c
UI = Unix/./
BIN_DIR = /usr/local/bin
MAN_DIR = /usr/local/man
INSTALL = install
INST_BIN_OPT = -s -o bin -g bin
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
	$(INSTALL) $(INST_BIN_OPT) tracker randomize analyzer splitmod $(BIN_DIR)
	$(INSTALL) $(INST_MAN_OPT) man/tracker.1 $(MAN_DIR)/man1/tracker.1

tracker: ${OBJ_TRACKER}
	${CC} -o tracker ${CFLAGS} ${OBJ_TRACKER} ${LIBS}

analyzer: ${OBJ_ANALYZER}
	$(CC) -o analyzer ${CFLAGS} ${OBJ_ANALYZER} ${LIBS}

randomize: randomize.c
	$(CC) -o randomize ${CFLAGS} randomize.c 

splitmod: ${OBJ_SPLITMOD}
	$(CC) -o splitmod ${CFLAGS} ${OBJ_SPLITMOD} ${LIBS}

main$O: song.h

$(AUDIO)audio$O: $(AUDIO)audio.c 
	cd $(AUDIO); $(CC) -I.. ${COPTS} audio.c

$(UI)ui$O: $(UI)ui.c
	cd $(UI); $(CC) -I.. ${COPTS} ui.c

automaton$O: song.h channel.h

getopt$O: getopt.h

getopt1$O: getopt.h

player$O: song.h channel.h

st_read$O: song.h

commands$O: channel.h song.h

clean:
	-rm -f config.h $(OBJ_TRACKER) $(OBJ_ANALYZER) $(OBJ_SPLITMOD) randomize$O
	-rm -f tracker randomize splitmod analyzer core*
