/* Copyright (C) 1989, 1990, 1993 Aladdin Enterprises.  All rights reserved.

This file is part of Ghostscript.

Ghostscript is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
to anyone for the consequences of using it or for whether it serves any
particular purpose or works at all, unless he says so in writing.  Refer
to the Ghostscript General Public License for full details.

Everyone is granted permission to copy, modify and redistribute
Ghostscript, but only under the conditions described in the Ghostscript
General Public License.  A copy of this license is supposed to have been
given to you along with Ghostscript so you can know your rights and
responsibilities.  It should be in a file named COPYING.  Among other
things, the copyright notice and this notice must be preserved on all
copies.  */

/* gsio.h */
/* stdio redirection for Ghostscript library */

/* Ghostscript never uses stdin/out/err directly. */
extern FILE *gs_stdin, *gs_stdout, *gs_stderr;

/* Redefine all the relevant stdio functions to use the above. */
/* Some functions we make illegal, rather than redefining them. */
#undef stdin
#define stdin gs_stdin
#undef stdout
#define stdout gs_stdout
#undef stderr
#define stderr gs_stderr
#undef fgetchar
#define fgetchar() fgetc(stdin)
#undef fputchar
#define fputchar(c) fputc(c, stdout)
#undef getchar
#define getchar() getc(stdin)
#undef gets
#define gets Function._gets_.unavailable
/* We should do something about perror, but since many Unix systems */
/* don't provide the strerror function, we can't.  (Neither the */
/* Ghostscript kernel nor any Aladdin-maintained driver uses perror.) */
#undef printf
#define printf Function._printf_.unavailable
#undef putchar
#define putchar(c) fputc(c, stdout)
#undef puts
#define puts(s) (fputs(s, stdout), putchar('\n'))
#undef scanf
#define scanf Function._scanf_.unavailable
#undef vprintf
#define vprintf Function._vprintf_.unavailable
#undef vscanf
#define vscanf Function._vscanf_.unavailable
