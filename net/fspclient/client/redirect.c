#include "client.h"
#include "main.h"
#include "redirect.h"
#include <ctype.h>

iobuffers global_iobuffers;

/*
** association between FILE streams and their numbers
**
**	0	STDIN
**	1	STDOUT
**	2	STDERR
**	3	STDPROMPT
**	4	STDINFO
**	5	STDWARN
**	6	STDDBG
**
** there is also a pseudo output called `all' which refers to all 1--6
*/

typedef struct FILE_ENTRY
{
    char	*name;
    FILE	*stream;
} FILE_ENTRY;

static FILE_ENTRY stream_lookup[] =
{
    { "in",		(FILE*)0	},
    { "out",		(FILE*)0	},
    { "err",		(FILE*)0	},
    { "prompt",		(FILE*)0	},
    { "info",		(FILE*)0	},
    { "warn",		(FILE*)0	},
    { "dbg",		(FILE*)0	},
    { "all",		(FILE*)0	},
    { (char*)0,		(FILE*)0	}
};

/*
** syntax:
**	if OUT is one of the output streams, and IN one of the input ones
**	then
**		ls OUT>file
**	redirects the stream called `OUT' to file `file'.  Similarly,
**		ls IN<file
**	does the appropriate redirection for the input stream(s).
**
**	If IN is omitted, then it defaults to "in"
**	If OUT is omitted, then it defaults to "out"
**
**	Following the convention of the Bourne Shell, IN<<xxx reads from 
**	IN until `xxx' is encountered.
**	Also, OUT1>&OUT2 causes the current stream associated with OUT2
**	to be made the stream of OUT1
*/

#define IS_INPUT(index)		(index == 0)
#define IS_OUTPUT(index)	(index != 0)

#if 0
static FILE *
choose_stream(fd, type, defaultfs)
    int fd;
    char *type;
    FILE *defaultfs;
{
    struct stat fdstat;

    if (fstat(fd, &fdstat) < 0)
	return defaultfs;
    else
	return fdopen(fd, type);
}
#endif

/*
** under Linux (and, I think, AIX) some file descriptors are left open with
** and executing binary -- these will tend to be 3, 4, etc.
** This cocks-up the idea of testing where to send output to... since output
** will get redirected to these (probably read-only) fd's.
**
** I'll do it properly sometime by passing command line arguments saying
** which file descriptors should be used for certain operations.
**
** For now: just return the default fd.
*/
static FILE *
#ifndef ANSI_PROTOTYPES
choose_stream(fd, type, defaultfs)
    int fd;
    char *type;
    FILE *defaultfs;
#else /* ANSI_PROTOTYPES */
choose_stream(int fd, char *type, FILE *defaultfs)
#endif /* ANSI_PROTOTYPES */
{
    return defaultfs;
}

/*
** initialise file descriptors
*/
void
#ifndef ANSI_PROTOTYPES
initialise_stdio()
#else /* ANSI_PROTOTYPES */
initialise_stdio(void)
#endif /* ANSI_PROTOTYPES */
{
    STDIN     = stdin;
    STDOUT    = stdout;
    STDERR    = stderr;

    STDDBG    = choose_stream(6, "a+", STDERR);

    if (isatty(fileno(STDIN)))
    {
	STDPROMPT = choose_stream(3, "a+", STDOUT);
	STDINFO   = choose_stream(4, "a+", STDOUT);
    }
    else
    {
	STDPROMPT = 0;
	STDINFO   = choose_stream(4, "a+", STDERR);
    }

    STDWARN   = choose_stream(5, "a+", STDERR);

    stream_lookup[0].stream = STDIN;
    stream_lookup[1].stream = STDOUT;
    stream_lookup[2].stream = STDERR;
    stream_lookup[3].stream = STDPROMPT;
    stream_lookup[4].stream = STDINFO;
    stream_lookup[5].stream = STDWARN;
    stream_lookup[6].stream = STDDBG;
}

#if 0
/*
** return 0 if no redirection in this word, 1 if there is and it has been
** processed, -1 if there is an error in the redirection
*/
int
process_redirection(word)
    char *word;
{
    char *redirect;
    char *p;
    int i;

    for (p = word, redirect = 0; *p && !redirect; p++)
	if (*p == '<' || *p == '>')
	    redirect = p;

    if (!redirect)
	return 0;

    i = 0;
    while (stream_lookup[i].name)
    {
	;
    }
}
#endif
