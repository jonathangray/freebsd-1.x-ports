/*
 *  Copyright (C) 1992, 1993 Søren Schmidt
 *
 *  This program is free software; you may redistribute it and/or 
 *  modify it, provided that it retain the above copyright notice 
 *  and the following disclaimer.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 *
 *	Søren Schmidt 		Email:	sos@kmd-ac.dk
 *	Tritonvej 36		UUCP:	...uunet!dkuug!kmd-ac!sos
 *	DK9210 Aalborg SO	Phone:  +45 9814 8076
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <machine/console.h>

main(int argc, char **argv)
{
int size;
char buffer[4096];
FILE *fd;

	if (argc == 3) {
		size = atoi(argv[1]);
		if (size != 8 && size != 16) {
			printf("%s: only sizes of 8 & 16 allowed\n", argv[0]);		
			exit(1);
		}
		fd = fopen(argv[2], "r");
		if (fd == NULL) {
			printf("%s: file not found\n", argv[0]);		
			exit(1);
		}
		fread(&buffer, size*256, 1, fd);
		if (size == 8) 
			ioctl(0, PIO_FONT8x8, buffer);
		else
			ioctl(0, PIO_FONT8x16, buffer);
		fclose(fd);
	}
	else
		printf("usage: setfont size file\n");
}
