# the makefile is not too well tested yet
CFLAGS=-O2
LBIN_DIR=/usr/local/bin
LCAT_DIR=/usr/local/man/cat1

all:posix crc minirb

posix:
	$(CC) $(CFLAGS) -DPOSIX -DMD=2 rz.c -o rz
	$(CC) $(CFLAGS) -DPOSIX sz.c -o sz

crc:
	$(CC) $(CFLAGS) crc.c -o crc

minirb:
	$(CC) $(CFLAGS) minirb.c -o minirb

install:
	install -cs -o bin -g bin -m 755 rz $(LBIN_DIR)
	rm -f $(LBIN_DIR)/rx $(LBIN_DIR)/rb $(LBIN_DIR)/rc
	ln $(LBIN_DIR)/rz $(LBIN_DIR)/rx
	ln $(LBIN_DIR)/rz $(LBIN_DIR)/rb
	ln $(LBIN_DIR)/rz $(LBIN_DIR)/rc
	install -cs -o bin -g bin -m 755 sz $(LBIN_DIR)
	rm -f $(LBIN_DIR)/sx $(LBIN_DIR)/sb $(LBIN_DIR)/zcommand $(LBIN_DIR)/zcommandi
	ln $(LBIN_DIR)/sz $(LBIN_DIR)/sx
	ln $(LBIN_DIR)/sz $(LBIN_DIR)/sb
	ln $(LBIN_DIR)/sz $(LBIN_DIR)/zcommand
	ln $(LBIN_DIR)/sz $(LBIN_DIR)/zcommandi
	install -c  -o bin -g bin -m 755 gz $(LBIN_DIR)
	install -cs -o bin -g bin -m 755 crc $(LBIN_DIR)
	install -c -o bin -g bin -m 644 crc.doc $(LCAT_DIR)/crc.1
	install -c -o bin -g bin -m 644 minirb.doc $(LCAT_DIR)/minirb.1
	install -c -o bin -g bin -m 644 rz.doc $(LCAT_DIR)/rz.1
	install -c -o bin -g bin -m 644 sz.doc $(LCAT_DIR)/sz.1

clean:
	rm -f core* rz sz crc minirb
