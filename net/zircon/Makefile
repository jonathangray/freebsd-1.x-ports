install: check
	install -c zircon /usr/local/bin
	@if [ ! -f ${HOME}/.zirconrc ]; then \
		echo ; \
		echo "Installed new .zirconrc in ${HOME} - please customize it for yourself"; \
		echo "before running zircon.  Also look at zircon.ad for possible"; \
		echo "inclusion into your .Xdefaults file"; \
		echo ; \
		install -c DOTzirconrc ${HOME}/.zirconrc ; \
	fi

check:
	@if [ ! -f /usr/local/bin/dpwish ]; then \
		echo "You must first install dpwish before this package!" ; \
		exit 1; \
	fi
