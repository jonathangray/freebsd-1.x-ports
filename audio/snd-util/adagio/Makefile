# Makefile for adagio
# Greg Lee, 5/6/93

DESTDIR = /usr/local/bin
MANDIR1 = /usr/local/man/man1
MANDIR3 = /usr/local/man/man3
ETCDIR = /usr/local/etc/midi
LIBDIR = /usr/local/lib/midi
# Change this to the directory where you keep midi files.
MIDIHOME = -DMIDIDIR=\"$(LIBDIR)/songs\"
# Change this to the directory where you keep fm instruments.
PATCHHOME = -DPDIR=\"$(LIBDIR)/sbi\"
# Change this to the directory where you keep gus patches.
GUSHOME = -DGUSPATDIR=\"$(LIBDIR)/patch\"
GUS2HOME = -DGUSPATDIR2=\"$(LIBDIR)/patch2\"

INST_OWNER = -o bin -g bin
LINK = cc
LEX = lex

# The value of XSELECT determines which channels get sent
# to the external midi port.  If b0 is 1, e.g., channel one
# gets sent there.  I have it as -DXSELECT=0x0ff to send the
# first eight channels out and play only the high eight channels
# on the sound card.  If you don't have an external port, you could
# put -DXSELECT=0.  Similarly, -DPSELECT determines which channels
# should be considered percussion channels; -DPSELECT marks
# channels 10 and 16 for percussion.
# -DK1 is for hooking up and external Kawai K1 -- the relevant code
# is in file phase2.c
# If the synth hooked to the external port has a small number of polyphonic
# voices, you can try adding -DXPOLYPHONY=8 or some other small number,
# in which case mp will disable external output to one or more channels
# if it thinks the limit you set will be exceeded (but I haven't been
# very succesful with this).
# See INSTALL for details on flags.

# Use this to build an xmp which does not use shared memory:
VERSIONFLAGS = -DXMP
# Use this to build an xmp which uses shared memory:
#VERSIONFLAGS = -DXMP -DUSE_SHM

# Distribution default flags:
CFLAGS = -O2 -DUNIX -DREAD_MODS \
	-DXVOICE -DSBDIR=\"$(ETCDIR)\" \
	-DWPOLYPHONY $(PATCHHOME) $(GUSHOME) $(GUS2HOME)

# Flags for use with Kawaii K1:
#CFLAGS = -Wall -O2 -DUNIX -DXSELECT=0x00ff -DPSELECT=0x8200 \
#       -DK1 -DXMAXCHAN=8 -DMIDIIN -DXVOICE -DXPOLYPHONY=8 \
#       -DWPOLYPHONY -DREAD_MODS $(PATCHHOME) $(GUSHOME) $(GUS2HOME)


# Use these libs to build xmp without shared memory, or if the
# shared memory calls are in libc:
XLDLIBS = -lsspkg -lxview -lolgx -lX11 -lm
# Use these libs for xmp with shared memory in case the shared
# memory calls are not in libc:
#XLDLIBS = -lipc -lsspkg -lxview -lolgx -lX11 -lm


XCFLAGS = $(CFLAGS) $(VERSIONFLAGS) $(MIDIHOME)
LDFLAGS =

SHROBJ = cardinit.o phase2.o allphase.o gusvoice.o cmdline.o userio.o \
	extvoice.o midifile.o cfg.o
ADOBJ = ad.o phase1.o $(SHROBJ)
MPOBJ =	mp.o phasem.o $(SHROBJ)
XMPOBJ = xmp.o phasem.o cardinit.o phase2x.o allphase.o gusvoice.o \
	extvoice.o midifile.o cfg.o
MGROBJ = pmgr.o cardinit.o allphase.o gusvoice.o cmdline.o cfg.o dummyext.o

EXES    = mp ad midt tm tracks setfm pmgr # xmp
MAN1    = mp.1 ad.1 tm.1 tracks.1 setfm.1 # xmp.1
MAN3    = midifile.3

all:    $(EXES)

pmgr:	$(MGROBJ)
	$(LINK) $(LDFLAGS) $(MGROBJ) -o $@

mp:	$(MPOBJ)
	$(LINK) $(LDFLAGS) $(MPOBJ) -o $@

xmp:	$(XMPOBJ)
	$(LINK) $(LDFLAGS) $(XMPOBJ) $(XLDLIBS) -o $@

ad:	$(ADOBJ)
	$(LINK) $(LDFLAGS) $(ADOBJ) -o $@

xmp.o: xmp.c
	$(CC) $(XCFLAGS) -c xmp.c -o xmp.o

phase2x.o: phase2.c
	$(CC) $(XCFLAGS) -c phase2.c -o phase2x.o

_midifile.o: midifile.c
	$(CC) -O2 -c midifile.c -o _midifile.o

cfg.o:	cfg.c
	$(CC) $(CFLAGS) -c cfg.c
cfg.c:	cfg.l

tm:     tm.c _midifile.o allphase.o
	$(CC) -O2 $(LDFLAGS) $@.c _midifile.o allphase.o -o $@

midt:   midt.c _midifile.o allphase.o
	$(CC) -O2 $(LDFLAGS) $@.c _midifile.o allphase.o -o $@

tracks: tracks.c
	$(CC) -O2 $(LDFLAGS) $@.c -o $@

setfm:	setfm.o
	$(LINK) $(LDFLAGS) setfm.o -o setfm

install: all
	install -c -s $(INST_OWNER) $(EXES) $(DESTDIR)
	-@mkdir -p $(MANDIR1) $(MANDIR3) $(ETCDIR) $(LIBDIR)
	install -c $(INST_OWNER) -m 644 std.* drums.* $(ETCDIR)
	install -c $(INST_OWNER) -m 644 oldfm.cfg $(ETCDIR)/fm.cfg
	install -c $(INST_OWNER) -m 644 oldext.cfg $(ETCDIR)/external.cfg
	install -c $(INST_OWNER) -m 644 oldpat.cfg $(ETCDIR)/default.cfg
	install -c $(INST_OWNER) -m 644 $(MAN1) $(MANDIR1)
	-@rm -f $(MANDIR1)/midt.1
	ln $(MANDIR1)/tm.1 $(MANDIR1)/midt.1
	install -c $(INST_OWNER) -m 644 $(MAN3) $(MANDIR3)

clean:
	rm -f *.o tm.c cfg.c
	rm -f ad midt xmp mp setfm tm tracks pmgr

ad.o: adagio.h userio.h cmdline.h allphase.h
phase1.o: admp.c adagio.h cmdline.h allphase.h
phase2.o: adagio.h userio.h cmdline.h pitch.h midicode.h midi.h \
	allphase.h vibrato.h
phase2x.o: phase2.c adagio.h userio.h cmdline.h pitch.h midicode.h midi.h \
	allphase.h vibrato.h
cardinit.o: allphase.h
userio.o: userio.h
cmdline.o: cmdline.h
allphase.o: allphase.h
phasem.o: admp.c adagio.h midifile.h midi.h midicode.h allphase.h
mp.o: adagio.h midifile.h allphase.h
xmp.o: adagio.h midifile.h allphase.h dot.bits xmp.bits
gusvoice.o: allphase.h gusvoice.h
extvoice.o: adagio.h allphase.h k1voice.h
cfg.o: adagio.h allphase.h
midifile.o: midifile.h mp_mod.c
_midifile.o: midifile.c midifile.h mp_mod.c
