NAME=appendfile
DESTDIR=/ai/lib/mail/bin
MANEXT=1
MANDIR=/ai/man/man$(MANEXT)
CFLAGS=-O

all: $(NAME)
$(NAME): $(NAME).c
	cc $(CFLAGS) $(NAME).c -o $(NAME)

install: all
	install -s -m 755 $(NAME) $(DESTDIR)
#	install -c -m a=r $(NAME).man $(MANDIR)/$(NAME).$(MANEXT)

clean:
	rm -f *.o $(NAME) \#* *~
