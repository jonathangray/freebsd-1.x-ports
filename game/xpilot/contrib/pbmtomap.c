/*
 * Written by Greg Renda (greg@ncd.com)
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define SIZE 100
#define TOK " \t\r\n"

static void
fatalError(message, arg1, arg2)
char           *message, *arg1, *arg2;
{
    fprintf(stderr, message, arg1, arg2);
    fprintf(stderr, ".\n");
    exit(1);
}

#define NEXT_TOKEN()							       \
    state++;								       \
    if (!(p = strtok(NULL, TOK)))					       \
        break;

int
main()
{
    char            buf[SIZE], *p, *n;
    int             state = 0, width, height, count;

    while (fgets(buf, SIZE, stdin))
    {
	p = strchr(buf, '#');
	if (p)
	    *p = 0;

	if (!(p = strtok(buf, TOK)))
	    continue;

	switch (state)
	{
	    case 0:
		if (strcmp(p, "P1"))
		    fatalError("Unrecognized input format");
		NEXT_TOKEN();
	    case 1:
		width = atoi(p);
		NEXT_TOKEN();
	    case 2:
		height = atoi(p);
		printf("%dx%d\n", width, height);
		printf("0\n");
		printf("This map hasn't got a name, please give it one\n");
		printf("%s", (n = getenv("USER")) ? n : "Unknown");
		count = 0;
		NEXT_TOKEN();
	    default:
		if (!(count % width))
		    putchar('\n');

		while (*p)
		{
		    if (*p == '1')
		    {
			putchar('x');
			count++;
		    }
		    else if (*p == '0')
		    {
			putchar(' ');
			count++;
		    }

		    p++;
		}

		NEXT_TOKEN();
	}
    }
}
