/* 
 * Copyright (C) 1994 Mark Boyns (boyns@sdsu.edu)
 *
 * FvwmSound
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

/*
 * This module is based on FvwmModuleDebugger which has the following
 * copyright:
 *
 * This module, and the entire ModuleDebugger program, and the concept for
 * interfacing this module to the Window Manager, are all original work
 * by Robert Nation
 *
 * Copyright 1994, Robert Nation. No guarantees or warantees or anything
 * are provided or implied in any way whatsoever. Use this program at your
 * own risk. Permission to use this program for any purpose is given,
 * as long as the copyright is kept intact.
 */

#include "../../configure.h"
#include <stdio.h>
#include <signal.h>
#include <fcntl.h>
#include <string.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <unistd.h>
#include <ctype.h>
#include <stdlib.h>
#include <rplay.h>
#include "../../fvwm/module.h"
#include "../../version.h"

char	*MyName;
int	fd_width;
int	fd[2];
int	rplay_fd = -1;

void	Loop(int *fd);
void	process_message(unsigned long type,unsigned long *body);
char	*safemalloc(int length);
void	DeadPipe(int nonsense);
void	config(char *config_file);
void	done(int n);

RPLAY	*message_table[MAX_MESSAGES];
char	*messages[MAX_MESSAGES] =
{
	"",
	"toggle_paging",
	"new_page",
	"new_desk",
	"add_window",
	"raise_window",
	"lower_window",
	"configure_window",
	"focus_change",
	"destroy_window",
	"iconify",
	"deiconify",
	"window_name",
	"icon_name",
	"res_class",
	"res_name",
	"end_windowlist",
};

/*
 * Builtin sound tables.
 */
#define MAX_BUILTIN		2
#define BUILTIN_STARTUP		0
#define BUILTIN_SHUTDOWN	1
RPLAY	*builtin_table[MAX_BUILTIN];
char 	*builtin[MAX_BUILTIN] =
{
	"startup",
	"shutdown",
};

main(int argc, char **argv)
{
	char *temp, *s;

	if((argc != 6)&&(argc != 7))
	{
		fprintf(stderr,"%s Version %s should only be executed by fvwm!\n",MyName,
			VERSION);
		exit(1);
	}

	/* Save our program  name - for error messages */
	temp = argv[0];
	s=strrchr(argv[0], '/');
	if (s != NULL)
		temp = s + 1;

	MyName = safemalloc(strlen(temp)+2);
	strcpy(MyName,"*");
	strcat(MyName, temp);

	/* Dead pipe == Fvwm died */
	signal (SIGPIPE, DeadPipe);  
  
	fd[0] = atoi(argv[1]);
	fd[1] = atoi(argv[2]);

#ifndef NO_SYSCONF
	fd_width = sysconf(_SC_OPEN_MAX);
#else
	fd_width = getdtablesize();
#endif

	/*
	 * Read the sound configuration.
	 */
	config(argv[3]);

	/*
	 * Play the startup sound.
	 */
	if (builtin_table[BUILTIN_STARTUP])
	{
		rplay(rplay_fd, builtin_table[BUILTIN_STARTUP]);
	}
	
	Loop(fd);
}

/***********************************************************************
 *
 *  Procedure:
 *	config - read the sound configuration file.
 *
 ***********************************************************************/
void	config(char *config_file)
{
	FILE	*fp;
	char	buf[BUFSIZ];
	char	host[128];
	char	*message;
	char	*sound;
	int	volume = RPLAY_DEFAULT_VOLUME;
	int	priority = RPLAY_DEFAULT_PRIORITY;
	char	*p;
	int	i, found;
	
	fp = fopen(config_file, "r");
	if (fp == NULL)
	{
		done(1);
	}

	/*
	 * Intialize all the sounds.
	 */
	for (i = 0; i < MAX_MESSAGES; i++)
	{
		message_table[i] = NULL;
	}
	for (i = 0; i < MAX_BUILTIN; i++)
	{
		builtin_table[i] = NULL;
	}

	/*
	 * Use rplay_default_host as the default host.
	 */
	strcpy(host, rplay_default_host());
	
	while (fgets(buf, sizeof(buf), fp))
	{
		buf[strlen(buf)-1] = '\0';
		if (buf[0] != '*')
		{
			continue;
		}
		/*
		 * Search for *FvwmSound.
		 */
		if (strncasecmp(buf, MyName, strlen(MyName)) == 0)
		{
			p = strtok(buf, " \t");
			
			if (strcasecmp(p, "*FvwmSoundHost") == 0)
			{
				p = strtok(NULL, " \t");
				if (p && *p)
				{
					strcpy(host, p);
				}
			}
			else if (strcasecmp(p, "*FvwmSoundVolume") == 0)
			{
				p = strtok(NULL, " \t");
				if (p && *p)
				{
					volume = atoi(p);
				}
			}
			else if (strcasecmp(p, "*FvwmSoundPriority") == 0)
			{
				p = strtok(NULL, " \t");
				if (p && *p)
				{
					priority = atoi(p);
				}
			}		
			else
			{
				message = strtok(NULL, " \t");
				sound = strtok(NULL, " \t");
				
				if (!message || !*message || !sound || !*sound)
				{
					continue;
				}
				
				found = 0;
				
				/*
				 * First check for a builtin sound.
				 */
				for (i = 0; !found && i < MAX_BUILTIN; i++)
				{
					if (strcasecmp(message, builtin[i]) == 0)
					{
						builtin_table[i] = rplay_create(RPLAY_PLAY);
						rplay_set(builtin_table[i], RPLAY_APPEND,
							RPLAY_SOUND,	sound,
							RPLAY_PRIORITY,	priority,
							RPLAY_VOLUME,	volume,
							NULL);
						found++;
					}				
				}

				for (i = 0; !found && i < MAX_MESSAGES; i++)
				{
					if (strcasecmp(message, messages[i]) == 0)
					{
						message_table[i] = rplay_create(RPLAY_PLAY);
						rplay_set(message_table[i], RPLAY_APPEND,
							RPLAY_SOUND,	sound,
							RPLAY_PRIORITY,	priority,
							RPLAY_VOLUME,	volume,
							NULL);
						found++;
					}
				}
			}
		}
	}

	fclose(fp);

	rplay_fd = rplay_open(host);
	if (rplay_fd < 0)
	{
		done(1);
	}
}

/***********************************************************************
 *
 *  Procedure:
 *	Loop - wait for data to process
 *
 ***********************************************************************/
void Loop(int *fd)
{
	unsigned long header[3], *body;
	char *cbody;
	int body_length,count,count2=0, total;

	while(1)
	{
		if((count = read(fd[1],header,3*sizeof(unsigned long))) > 0)
		{
			if(header[0] == START_FLAG)
			{
				body_length = header[2]-3;
				body = (unsigned long *)
					safemalloc(body_length * sizeof(unsigned long));
				cbody = (char *)body;
				total = 0;
				while(total < body_length*sizeof(unsigned long))
				{
					if((count2=read(fd[1],&cbody[total],
						body_length*sizeof(unsigned long)-total)) >0)
						total += count2;
					else if(count2 < 0)
						DeadPipe(0);
				}
				/*
				 * Play the sound.
				 */
				if (message_table[header[1]])
				{
					rplay(rplay_fd, message_table[header[1]]);
				}
				free(body);
			}
		}
		if(count <= 0)
			DeadPipe(1);
	}
}

/***********************************************************************
 *
 *  Procedure:
 *	safemalloc - mallocs specified space or exits if there's a 
 *		     problem
 *
 ***********************************************************************/
char *safemalloc(int length)
{
	char *ptr;
  
	if(length <= 0)
		length = 1;

	ptr = malloc(length);
	if(ptr == (char *)0)
	{
		fprintf(stderr,"%s:malloc failed",MyName);
		exit(1);
	}
	return ptr;
}

/***********************************************************************
 *
 *  Procedure:
 *	SIGPIPE handler - SIGPIPE means fvwm is dying
 *
 ***********************************************************************/
void DeadPipe(int nonsense)
{
	done(0);
}

/***********************************************************************
 *
 *  Procedure:
 *	done - common exit point for FvwmSound.
 *
 ***********************************************************************/
void	done(int n)
{
	if (rplay_fd != -1 && builtin_table[BUILTIN_SHUTDOWN])
	{
		rplay(rplay_fd, builtin_table[BUILTIN_SHUTDOWN]);
	}	
	exit(n);
}
