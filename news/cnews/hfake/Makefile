I = ../include

# beware -- build knows about NEEDED
NEEDED =  ../include/string.h ../include/stdlib.h ../include/stddef.h

all:	$(NEEDED)

$(I)/string.h:	string.h
	cp string.h $@
$(I)/stddef.h:	stddef.h
	cp stddef.h $@
$(I)/stdlib.h:	stdlib.h
	cp stdlib.h $@
$(I)/sys/timeb.h:	timeb.h
	cp timeb.h $@

clean:
