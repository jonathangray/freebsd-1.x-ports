/* serverhost.c - getpwent() style interface to /etc/serverhosts */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char serverhost_c_sccsid[] = "@(#)serverhost.c	1.6 26/4/92 (UKC)";

#include <sys/types.h>
#include <sys/file.h>
#include <stdio.h>
#include <string.h>
#include <local/ukcprog.h>

#include "serverhost.h"

static const char Sh_filename[] = "/etc/serverhosts";
static FILE *Sfp;
static int Lnum;
static serverhost_status_t Sh_status = SH_CLOSED;

#define MAX_HOSTS	10

serverhost_status_t
getserverhoststatus()
{
	return Sh_status;
}

int
setserverhostent()
{
	if (Sfp == NULL) {
		if ((Sfp = fopen(Sh_filename, "r")) == NULL) {
			errf("Can't open %s (%m)", Sh_filename);
			return -1;
		}
	}
	else {
		if (fseek(Sfp, 0L, L_SET) == EOF) {
			errf("Can't fseek in %s (%m)", Sh_filename);
			return -1;
		}
		clearerr(Sfp);
	}
	Lnum = 0;
	Sh_status = SH_OPEN;
	return 0;
}

void
endserverhostent()
{
	(void) fclose(Sfp);
	Sfp = NULL;
	Sh_status = SH_CLOSED;
}

struct serverhostent *
getserverhostent()
{
	static const char *hostnames[MAX_HOSTS + 1];
	static char buf[256];
	static char *cptr;
	static struct serverhostent shbuf;
	int nhosts;

	do {
		if (fgets(buf, sizeof(buf), Sfp) == NULL) {
			Sh_status = ferror(Sfp) ? SH_ERROR : SH_EOF;
			if (ferror(Sfp))
				errf("Read error in %s (%m)", Sh_filename);
			return NULL;
		}
		++Lnum;

		if ((cptr = strchr(buf, '\n')) == NULL) {
			errf("Line %d too long in %s", Lnum, Sh_filename);
			return NULL;
		}
		*cptr = '\0';

		for (cptr = buf; *cptr == ' ' || *cptr == '\t'; ++cptr)
			;
	} while (*cptr == '#' || *cptr == '\0');
	
	shbuf.sh_servname = cptr;
	if ((cptr = strchr(cptr, ':')) == NULL) {
		errf("Missing ':' on line %d of %s", Lnum, Sh_filename);
		return NULL;
	}
	*cptr++ = '\0';

	nhosts = 0;
	for (;;) {
		while (*cptr == ' ' || *cptr == '\t')
			++cptr;
		if (*cptr == '\0' || *cptr == '#')
			break;
		if (nhosts == MAX_HOSTS) {
			errf("Too many hosts on line %d of %s (max %d)",
						Lnum, Sh_filename, MAX_HOSTS);
			return NULL;
		}
		hostnames[nhosts++] = cptr;
		while (*cptr != ' ' && *cptr != '\t' && *cptr != '\0')
			++cptr;
		if (*cptr == '\0')
			break;
		*cptr++ = '\0';
	}
	hostnames[nhosts] = NULL;
	shbuf.sh_hosts = hostnames;

	return &shbuf;
}

struct serverhostent *
getserverhostbyservname(servname)
const char *servname;
{
	struct serverhostent *sh;

	if (setserverhostent() != 0)
		return NULL;
	while ((sh = getserverhostent()) != NULL)
		if (strcmp(sh->sh_servname, servname) == 0)
			return sh;
	return NULL;
}
