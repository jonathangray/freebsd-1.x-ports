all:
	(cd src; make)

install: all
	install -c -s -m 755 vim /usr/local/bin
	-@mkdir -p /usr/local/doc
	install -c -m 644 doc/vim.hlp /usr/local/doc

clean:
	rm -f vim
	(cd src; make clean)
