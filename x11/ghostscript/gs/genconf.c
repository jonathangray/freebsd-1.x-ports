/* Copyright (C) 1993 Aladdin Enterprises.  All rights reserved.

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

/* genconf.c */
/* Generate configuration files for Ghostscript */
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>		/* for calloc */
#include <string.h>

/*
 * This program generates a set of configuration files for Ghostscript.
 * Everything it does could be done by a shell program, except that
 * (1) Unix shells are not compatible from one system to another,
 * (2) the DOS shell is not equal to the task,
 * (3) the VMS shell is radically different from all others.
 *
 * Usage:
 *	genconf [@]xxx.dev* [-f gconfigf.h] [-h gconfig.h]
 *	  [-l lib.tr] [-o obj.tr] [-u ld.tr] [-w objw.tr]
 */

/* This program has to use K&R C, so it will work with old compilers. */
#undef const
#define const /* */
#define P1(a1) /* */
#define P2(a1,a2) /* */
#define P3(a1,a2,a3) /* */
/* Unfortunately, we have to hack up the function definitions by hand. */

/* Structures for accumulating information */
typedef struct string_list_s {
	int max_count, count;
	char **strings;
} string_list;
typedef struct config_s {
	string_list resources;
	string_list devs;
	string_list fonts;
	string_list libs;
	string_list objs;
} config;
static const config init_config = {
	{ 200 }, { 100 }, { 200 }, { 100 }, { 200 }
};

/* Forward definitions */
int alloc_list(P1(string_list *));
int read_dev(P2(config *, const char *));
int read_token(P3(char *, int, FILE *));
int add_entry(P3(config *, char *, const char *));
void sort_uniq(P1(string_list *));
void write_list(P3(FILE *, const string_list *, const char *));

main(argc, argv) int argc; char *argv[];
{	config conf;
	int i;

	/* Allocate string lists. */
	conf = init_config;
	alloc_list(&conf.resources);
	alloc_list(&conf.devs);
	alloc_list(&conf.fonts);
	alloc_list(&conf.libs);
	alloc_list(&conf.objs);

	/* Process command line arguments. */
	for ( i = 1; i < argc; i++ )
	{	const char *arg = argv[i];
		FILE *out;
		if ( *arg != '-' )
		{	read_dev(&conf, arg);
			continue;
		}
		if ( i == argc - 1 )
		{	fprintf(stderr, "Missing file name after %s.\n",
				arg);
			exit(1);
		}
		out = fopen(argv[++i], "w");
		if ( out == 0 )
		{	fprintf(stderr, "Can't open %s for output.\n",
				argv[i]);
			exit(1);
		}
		switch ( arg[1] )
		{
		case 'f':
			fputs("/* This file was generated automatically by Ghostscript (genconf.c). */\n", out);
			write_list(out, &conf.fonts,
				   "font_(\"0.font_%s\",gsf_%s,zf_%s)\n");
			break;
		case 'h':
			fputs("/* This file was generated automatically by Ghostscript (genconf.c). */\n", out);
			fputs("#include \"gsconfig.h\"\n", out);
			write_list(out, &conf.devs, "device__(gs_%s_device)\n");
			sort_uniq(&conf.resources);
			write_list(out, &conf.resources, "%s\n");
			break;
		case 'l':
			write_list(out, &conf.libs, "%s+\n");
			break;
		case 'o':
			sort_uniq(&conf.objs);
			write_list(out, &conf.objs, "%s+\n");
			break;
		case 'u':
			sort_uniq(&conf.objs);
			write_list(out, &conf.objs, "%s \\\n");
			write_list(out, &conf.libs, "-l%s \\\n");
			break;
		case 'w':
			sort_uniq(&conf.objs);
			write_list(out, &conf.objs, "FILE %s\n");
			break;
		default:
			fclose(out);
			fprintf(stderr, "Unknown switch %s.\n", arg);
			exit(1);
		}
		fclose(out);
	}

	exit(0);
}

/* Allocate and initialize a string list. */
int
alloc_list(list) string_list *list;
{	list->count = 0;
	list->strings =
		(char **)calloc(list->max_count, sizeof(char *));
	return 0;
}

/* Read and parse a .dev file. */
int
read_dev(pconf, arg) config *pconf; const char *arg;
{	FILE *in;
#define max_token 32
	char token[max_token];
	int len;
	if ( arg[0] == '@' )
	{	in = fopen(arg + 1, "r");
		if ( in == 0 )
		{	fprintf(stderr, "Can't read %s.\n", arg + 1);
			exit(1);
		}
		while ( (len = read_token(token, max_token, in)) > 0 )
			read_dev(pconf, token);
	}
	else
	{	char category[max_token];
		in = fopen(arg, "r");
		if ( in == 0 )
		{	fprintf(stderr, "Can't read %s.\n", arg);
			exit(1);
		}
		strcpy(category, "obj");
		while ( (len = read_token(token, max_token, in)) > 0 )
			add_entry(pconf, category, token);
	}
#undef max_token
	fclose(in);
	if ( len < 0 )
	{	fprintf(stderr, "Token too long: %s.\n", token);
		exit(1);
	}
	return 0;
}

/* Read a token from a file. */
int
read_token(token, max_len, in) char *token; int max_len; FILE *in;
{	int len = 0;
	for ( ; ; ) 
	{	char ch = fgetc(in);
		token[len] = 0;
		if ( feof(in) )
			return len;
		if ( isspace(ch) )
		{	if ( len > 0 )
				return len;
			continue;
		}
		if ( len == max_len )
			return -1;	/* token too long */
		token[len++] = ch;
	}
}

/* Add an entry to a configuration. */
int
add_entry(pconf, category, item) config *pconf; char *category; const char *item;
{	if ( item[0] == '-' )		/* set category */
		strcpy(category, item + 1);
	else				/* add to current category */
	{	char str[80];
		const char *pat = "%s";
		char *rstr;
		string_list *list = &pconf->resources;
		/* Handle a few resources specially: */
		if ( !strcmp(category, "dev") )
			list = &pconf->devs;
		else if ( !strcmp(category, "font") )
			list = &pconf->fonts;
		else if ( !strcmp(category, "include") )
		{	strcpy(str, item);
			strcat(str, ".dev");
			read_dev(pconf, str);
			return 0;
		}
		else if ( !strcmp(category, "lib") )
			list = &pconf->libs;
		else if ( !strcmp(category, "obj") )
			list = &pconf->objs;
		/* Now handle all other resources. */
		else if ( !strcmp(category, "fdev") )
			pat = "file_device__(gs_fdev_%s)";
		else if ( !strcmp(category, "oper") )
			pat = "oper__(%s_op_defs)";
		else if ( !strcmp(category, "oper2") )
			pat = "oper2__(%s_op_defs)";
		else if ( !strcmp(category, "ps") )
			pat = "psfile__(\"%s.ps\")";
		else
		{	fprintf(stderr, "Unknown category %s.\n", category);
			exit(1);
		}
		sprintf(str, pat, item);
		rstr = malloc(strlen(str) + 1);
		strcpy(rstr, str);
		list->strings[list->count++] = rstr;
	}
	return 0;
}

/* Sort and uniq an array of strings. */
int
strcmp_ptr(ps1, ps2) const void *ps1; const void *ps2;
{	return strcmp(*(const char **)ps1, *(const char **)ps2);
}
void
sort_uniq(list) string_list *list;
{	char **strlist = list->strings;
	int count = list->count;
	char **from;
	char **to;
	int i;
	if ( count == 0 )
		return;
	qsort((char *)strlist, count, sizeof(char *), strcmp_ptr);
	for ( from = to = strlist + 1, i = 1; i < count; from++, i++ )
	  if ( strcmp(*from, to[-1]) )
	    *to++ = *from;
	list->count = to - strlist;
}

/* Write a list of strings using a template. */
void
write_list(out, list, template) FILE *out; const string_list *list; const char *template;
{	int i;
	for ( i = 0; i < list->count; i++ )
	{	const char *str = list->strings[i];
		/* The repetition of str is a hack to make the */
		/* fonts template work. */
		fprintf(out, template, str, str, str);
	}
}
