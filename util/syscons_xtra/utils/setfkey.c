/*
 *  Copyright (C) 1992, 1993 S�ren Schmidt
 *
 *  This program is free software; you may redistribute it and/or 
 *  modify it, provided that it retain the above copyright notice 
 *  and the following disclaimer.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 *
 *	S�ren Schmidt 		Email:	sos@kmd-ac.dk
 *	Tritonvej 36		UUCP:	...uunet!dkuug!kmd-ac!sos
 *	DK9210 Aalborg SO	Phone:  +45 9814 8076
 */

#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/console.h>

main(int argc, char **argv)
{
	fkeyarg_t key;

	if (argc == 3) {
		key.flen = strlen(argv[2]);
		if (key.flen > MAXFK) {
			printf("%s: string max %d chars long\n", argv[0], MAXFK);
			exit(1);
		}
		strcpy(key.keydef, argv[2]);
		key.keynum = atoi(argv[1]) - 1;
		ioctl(0, SETFKEY, &key);
	}
	else
		printf("usage: setfkey key# string\n");
}
