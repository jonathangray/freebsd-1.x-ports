/* 
 * Copyright (C) 1993 Andrew Scherpbier (Andrew@sdsu.edu)
 *
 * This file is part of xpmtp.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "xpmtp.h"

typedef struct
{
	char	*name;
	int		min_args;
	int		max_args;
#ifdef __STDC__
	void	(*func)(int argc, char **argv);
#else
	void	(*func)();
#endif
	char	*shorthelp;
	char	*longhelp;
} COMMAND;

#ifdef __STDC__
void	command_help(int argc, char **argv);
void	command_send(int argc, char **argv);
void	command_quit(int argc, char **argv);
void	command_put(int argc, char **argv);
void	command_get(int argc, char **argv);
void	command_geti(int argc, char **argv);
void	command_unknown(int argc, char **argv);
void	command_wildmap(int argc, char **argv);
void	argv_to_command(char **argv);
int		connected();
void	done(int exit_value);
void	usage();
#else
void	command_help(/* int argc, char **argv */);
void	command_send(/* int argc, char **argv */);
void	command_quit(/* int argc, char **argv */);
void	command_put(/* int argc, char **argv */);
void	command_get(/* int argc, char **argv */);
void	command_geti(/* int argc, char **argv */);
void	command_unknown(/* int argc, char **argv */);
void	command_wildmap(/* int argc, char **argv */);
void	argv_to_command(/* char **argv */);
int		connected();
void	done(/* int exit_value */);
void	usage();
#endif

COMMAND	commands[] =
{
	"help",		0,	XPMTP_MAX_ARGS,	command_help,	"Display this list of commands.  HELP HELP will give more info",
"usage: help [command]\n\
Without the command argument, the HELP command will list all available commmands\n\
with a short description.  By specifying the command, you get more detailed help",
	"quit",		0,	0,				command_quit,	"Quit the connection with the server",
"usage: quit\n\
This will close the connection with the map server and quit the client program.",
	"get",		1,	2,				command_get,	"Get a map",
"usage: get mapname [localmapname]\n\
This is used to retrieve a map from the map server.  The optional localmapname\n\
specifies the name you want the map to have on your system.",
	"geti",		1,	2,				command_geti,	"Get a PBM image of a map.",
"usage: geti mapname [localpbmfilename]\n\
Use this to retrieve a small (64x64) pbm image representation of the specified\n\
map.  If no localpbmfilename is specified, the mapname is used with the .map\n\
substitued with .pbm",
	"submit",	1,	2,				command_put,	"Submit a map to the server for others to use",
"usage: submit localmapname [remotemapname]\n\
Send a map to the map server.  The localmapname is the name you use on your\n\
system. If you don't specify a remote map name, the local map name will be used.\n\
Note that you CANNOT overwrite an existing map.  If you feel you need to\n\
replace a map, contact the map server maintainer.  (See the maintainer command)",
	"list",		0,	2,				command_send,	"List the currently available maps.",
"usage: list [-l] [mapname]\n\
List one or all maps in the archive.  If mapname is specified, only that map is\n\
listed.  If the -l option is specified, more information about each map is\n\
returned.  The fields in this long list are tab seperated so they are easy to\n\
parse with a program.",
	"display",	1,	3,				command_send,	"Display image of map in a X window.",
"usage: display [-display machine:0.0] mapname\n\
Create an X window on the specified display.  If no display is explicitly\n\
specified the machine you are connecting from will be used.\n\
The image displayed is a small (128x128) image representation of the specified\n\
map.  This gives you a quick overview of what the map looks like.  Remember\n\
that ALL maps are displayed in the same size.  The aspect ratio is preserved,\n\
however.",
	"wildmap",	0,	XPMTP_MAX_ARGS,	command_wildmap,"Generate a random map using the 'wildmap' program and get it.",
"usage: wildmap [-options] [localmapname]\n\
This will create a random map using the wildmap command.  To get a list of the\n\
options to the wildmap program, use the 'wildmap -help' command.\n\
If no localmapname is specified, wildmap.map is used.",
	"maintainer",0,	0,				command_send,	"Show how to get in touch with the maintainer",
"usage: maintainer\n\
Show how to get in touch with the map server maintainer.\n\
You should contact this person if you are having problems of if you want to\n\
update/replace an existing map.  (You are not allowed to do that using XPMTP)",
};


#define NCOMMANDS	(sizeof(commands)/sizeof(COMMAND))

int		xpmtp_fd = -1;
char	xpmtp_buf[4096];
char	command[XPMTP_MAX_LINE];
char	response[XPMTP_MAX_LINE];

extern int	optind;
extern char	*optarg;

#ifdef __STDC__
main(int argc, char **argv)
#else
main(argc, argv)
int		argc;
char	**argv;
#endif
{
	char	buf[256];
	int		i;
	int		port = XPMTP_PORT;
	int		c;
	char	*av[XPMTP_MAX_ARGS], *p, *host = NULL;
	int		ac, first;

	while ((c = getopt(argc, argv, "h:p:")) != -1)
	{
		switch (c)
		{
			case 'h':
				host = optarg;
				break;
			case 'p':
				port = atoi(optarg);
				break;
			case '?':
				usage();
				done(0);
		}
	}

	if (optind != argc)
	{
		usage();
		done(1);
	}

	if (host == NULL)
	{
		host = xpmtp_default_host();
	}

	printf("Opening XPMTP connection with %s ...\n", host);
	xpmtp_fd = xpmtp_open(host, port, response, sizeof(response));
	if (xpmtp_fd < 0)
	{
		xpmtp_perror("open");
		done(1);
	}
	else
	{
		printf("Connected to %s port %d.\n", host, port);
		printf("%s\n", response+1);
	}

	for (;;)
	{
		printf("xpmtp> ");
		fflush(stdout);
		if (fgets(buf, sizeof(buf), stdin) == NULL)
		{
			command_quit(ac, av);
		}
		first = 1;
		ac = 0;
		while (p = strtok(first ? buf : NULL, " \t\r\n"))
		{
			av[ac++] = p;
			first = 0;
		}
		av[ac] = NULL;

		if (av[0] == NULL)
		{
			continue;
		}

		for (i = 0; i < NCOMMANDS; i++)
		{
			if (strcasecmp(commands[i].name, av[0]) == 0)
			{
				if (commands[i].min_args >= 0 && ac-1 < commands[i].min_args
					|| commands[i].max_args >= 0 && ac-1 > commands[i].max_args)
				{
					printf("wrong number of arguments.\n");
				}
				else
				{
					(*commands[i].func)(ac, av);
				}
				break;
			}
		}
		if (i == NCOMMANDS)
		{
			command_unknown(ac, av);
		}
	}
}

#ifdef __STDC__
void	argv_to_command(char **argv)
#else
void	argv_to_command(argv)
char	**argv;
#endif
{
	command[0] = '\0';
	for (; *argv; argv++)
	{
		strcat(command, *argv);
		if (*(argv+1))
		{
			strcat(command, " ");
		}
	}
}

#ifdef __STDC__
void	command_quit(int argc, char **argv)
#else
void	command_quit(argc, argv)
int	argc;
char	**argv;
#endif
{
	if (xpmtp_fd != -1)
	{
		xpmtp_close(xpmtp_fd);
		printf("Connection closed.\n");
	}
	done(0);
}

#ifdef __STDC__
void	command_help(int argc, char **argv)
#else
void	command_help(argc, argv)
int	argc;
char	**argv;
#endif
{
	int		i;
	int		j;
	int		found;

	if (argc == 1)
	{
		/*
		 * Short list
		 */
		for (i = 0; i < NCOMMANDS; i++)
			printf("%7s -- %s\n\n", commands[i].name, commands[i].shorthelp);
	}
	else
	{
		for (j = 1; j < argc; j++)
		{
			found = 0;
			for (i = 0; i < NCOMMANDS; i++)
			{
				if (strcmp(commands[i].name, argv[j]) == 0)
				{
					printf("%7s -- %s\n", commands[i].name, commands[i].shorthelp);
					printf("%s\n\n", commands[i].longhelp);
					found = 1;
					break;
				}
			}
			if (!found)
				printf("ERR Sorry, '%s' is not a recognized command\n", argv[j]);
		}
	}
}

#ifdef __STDC__
void	command_put(int argc, char **argv)
#else
void	command_put(argc, argv)
int	argc;
char	**argv;
#endif
{
	FILE		*fp;
	char		*filename;
	char		*remotefilename;
	char		buffer[1024];
	int			n;
	struct stat	buf;

	if (argc == 3)
		remotefilename = argv[2];
	else
		remotefilename = argv[1];
	filename = argv[1];

	/*
	 * Make sure that we aren't kidding about sending a map.  We need to first
	 * make sure that it really exists locally.
	 */
	if (stat(filename, &buf) == -1)
	{
		printf("ERR: You are trying to send a file which does not exist!\n");
		return;
	}
	fp = fopen(filename, "r");
	if (fp == NULL)
	{
		printf("ERR: Unable to open %s for reading.\n", filename);
		return;
	}

	argv[1] = remotefilename;
	argv[2] = NULL;
	argv_to_command(argv);

	switch (xpmtp_command(xpmtp_fd, command, response, sizeof(response)))
	{
		case -1:
			xpmtp_perror(argv[0]);
			command_quit(argc, argv);
			return;
		default:
			if (*response == '-')
			{
				printf("%s\n", response+1);
				return;
			}
	}

	/*
	 * We are now ready to send the map over.
	 * First the size, then the map, just like the server were to send a map to
	 * us.
	 */
	xpmtp_putline(xpmtp_fd, "#%d", buf.st_size);

	n = 1;
	while (n > 0)
	{
		n = fread(buffer, 1, 1024, fp);
		if (n == 0)
			break;				/* All done writing */
		n = xpmtp_write(xpmtp_fd, buffer, n);
		if (n <= 0)
		{
			/*
			 * Some problem arized!
			 */
			printf("ERR: Hmmm...  Problems writing\n");
			return;
		}
	}

	fclose(fp);
	printf("OK Submitted map '%s'\n", remotefilename);
}

#ifdef __STDC__
void	command_get(int argc, char **argv)
#else
void	command_get(argc, argv)
int	argc;
char	**argv;
#endif
{
	FILE	*fp;
	char	*filename;
	char	*p;
	int	size, n, nread;
	char	cmd[100];
	
	if (argc == 3)
	{
		filename = argv[2];
		argv[2] = NULL;
	}
	else
	{
		filename = argv[1];
	}

	argv_to_command(argv);

	switch (xpmtp_command(xpmtp_fd, command, response, sizeof(response)))
	{
		case -1:
			xpmtp_perror(argv[0]);
			command_quit(argc, argv);
			return;
		default:
			if (*response == '-')
			{
				printf("%s\n", response+1);
				return;
			}
	}

	size = atoi(response + 1);

	fp = fopen(filename, "w");
	if (fp == NULL)
	{
		perror(filename);
		return;
	}

	while (size > 0)
	{
		n = MIN(sizeof(xpmtp_buf), size);
		nread = xpmtp_read(xpmtp_fd, xpmtp_buf, n);
		if (nread != n)
		{
			xpmtp_perror("get");
			break;
		}
		fwrite(xpmtp_buf, 1, n, fp);
		size -= n;
	}

	fclose(fp);
	printf("OK Retrieved map:\n");
	sprintf(cmd, "ls -l %s", filename);
	system(cmd);
}

#ifdef __STDC__
void	command_geti(int argc, char **argv)
#else
void	command_geti(argc, argv)
int	argc;
char	**argv;
#endif
{
	FILE	*fp;
	char	*filename;
	char	*p;
	int		size, n, nread;
	char	newname[200];
	char	cmd[100];

	if (argc == 3)
	{
		filename = argv[2];
		argv[2] = NULL;
	}
	else
	{
		strcpy(newname, argv[1]);
		p = strrchr(newname, '.');
		if (p)
		{
			*p = '\0';
		}
		strcat(newname, ".pbm");
		filename = newname;
	}

	argv_to_command(argv);

	printf("Getting image for %s.  Image will be stored in %s\n", argv[1], filename);

	switch (xpmtp_command(xpmtp_fd, command, response, sizeof(response)))
	{
		case -1:
			xpmtp_perror(argv[0]);
			command_quit(argc, argv);
			return;
		default:
			if (*response == '-')
			{
				printf("%s\n", response+1);
				return;
			}
	}

	size = atoi(response + 1);

	fp = fopen(filename, "w");
	if (fp == NULL)
	{
		perror(filename);
		return;
	}

	while (size > 0)
	{
		n = MIN(sizeof(xpmtp_buf), size);
		nread = xpmtp_read(xpmtp_fd, xpmtp_buf, n);
		if (nread != n)
		{
			xpmtp_perror("get");
			break;
		}
		fwrite(xpmtp_buf, 1, n, fp);
		size -= n;
	}

	fclose(fp);
	printf("OK Retrieved image:\n");
	sprintf(cmd, "ls -l %s", filename);
	system(cmd);
}

#ifdef __STDC__
void	command_send(int argc, char **argv)
#else
void	command_send(argc, argv)
int	argc;
char	**argv;
#endif
{
	char	*p;
	
	argv_to_command(argv);

	switch (xpmtp_command(xpmtp_fd, command, response, sizeof(response)))
	{
		case -1:
			xpmtp_perror(argv[0]);
			command_quit(argc, argv);
			return;
		default:
			switch (*response)
			{
				case '-':
				case '+':
					printf("%s\n", response+1);
					break;
				case '*':
					printf("%s\n", response+1);
					while (xpmtp_getline(xpmtp_fd, response, sizeof(response)) >= 0)
					{
						if (*response == '.')
							break;
						printf("%s\n", response);
					}
					break;
				default:
					printf("%s\n", response);
					/*
					 * There are some errors...  List until we find either an +OK or -ERR
					 */
					while (xpmtp_getline(xpmtp_fd, response, sizeof(response)) >= 0)
					{
						if (*response == '-' || *response == '+' || *response)
						{
							printf("%s\n", response+1);
							return;
						}
						printf("%s\n", response);
					}
					break;
			}
			return;
	}

}

#ifdef __STDC__
void	command_unknown(int argc, char **argv)
#else
void	command_unknown(argc, argv)
int	argc;
char	**argv;
#endif
{
	printf("Unknown command '%s'.\n", argv[0]);
}

#ifdef __STDC__
void	command_wildmap(int argc, char **argv)
#else
void	command_wildmap(argc, argv)
int	argc;
char	**argv;
#endif
{
	FILE	*fp;
	char	*filename = "wildmap.map";
	char	*p;
	int	size, n, nread;
	char	cmd[100];
	
	if (argc != 1)
	{
		filename = argv[argc - 1];
		argc--;
	}

	argv_to_command(argv);

	switch (xpmtp_command(xpmtp_fd, command, response, sizeof(response)))
	{
		case -1:
			xpmtp_perror(argv[0]);
			command_quit(argc, argv);
			return;
		case '*':
			printf("%s\n", response+1);
			while (xpmtp_getline(xpmtp_fd, response, sizeof(response)) >= 0)
			{
				if (*response == '.')
					break;
				printf("%s\n", response);
			}
			return;
		case '-':
			printf("%s\n", response+1);
			return;
	}

	size = atoi(response + 1);

	fp = fopen(filename, "w");
	if (fp == NULL)
	{
		perror(filename);
		return;
	}

	while (size > 0)
	{
		n = MIN(sizeof(xpmtp_buf), size);
		nread = xpmtp_read(xpmtp_fd, xpmtp_buf, n);
		if (nread != n)
		{
			xpmtp_perror("get");
			break;
		}
		fwrite(xpmtp_buf, 1, n, fp);
		size -= n;
	}

	fclose(fp);
	printf("OK Retrieved map:\n");
	sprintf(cmd, "ls -l %s", filename);
	system(cmd);
}

void	usage()
{
	printf("usage: xpmtp [options]\n");
	printf("\t-h host   Specify the XPMTP host, default = %s.\n",
		xpmtp_default_host());
	printf("\t-p port   Use port instead of the default XPMTP port, default = %d.\n", XPMTP_PORT);
	printf("\n");
}

#ifdef __STDC__
void	done(int exit_value)
#else
void	done(exit_value)
int	exit_value;
#endif
{
	exit(exit_value);
}
