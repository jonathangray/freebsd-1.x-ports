#include <stdio.h>
#include <string.h>
#include <malloc.h>

/* Pause this many seconds between retrying the command */
#define SLEEP_TIME	2

main (argc, argv)
    int argc;
    char **argv;
{
    char *cmd, *s;
    int i, len;

    /* Figure out how much buffer space to alloc. */
    len = 0;
    for (i=1; i<argc; i++) {
	len += strlen(argv[i]);
    }
    cmd = (char *)malloc(len+1+argc);

    /* Build the command */
    s = cmd;
    for (i=1; i<argc; i++) {
	strcpy (s, argv[i]);
	s += strlen(argv[i]);
	if (i != (argc-1)) {
	   *s++ = ' ';
	}
    }

    while (1) {
	system(cmd);
	sleep (SLEEP_TIME);
    }
}
