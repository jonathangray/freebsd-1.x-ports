CFLAGS  = -O

all:            mixer

install:
	install -c -s mixer /usr/local/bin

mixer:          mixer.o
		$(CC) -o mixer mixer.o

clean:
	rm -f mixer *.o
