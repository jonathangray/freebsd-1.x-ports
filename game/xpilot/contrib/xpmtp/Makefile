CC		=	gcc
CFLAGS	=	-g
OBJS	=	xpmtp.o libxpmtp.o
BINDIR	=	/usr/local/bin
TARGET	=	xpmtp

$(TARGET):	$(OBJS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $(TARGET) $(OBJS) $(LIBS)

install:	$(TARGET)
	install -s xpmtp $(BINDIR)

clean:
	rm -f $(OBJS) $(TARGET)

shar:
	shar -c Makefile *.c *.h >xpmtp.shar
