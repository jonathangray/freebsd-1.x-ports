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

#include <sys/types.h>
#include <sys/ioctl.h>
#include <machine/console.h>
#include <signal.h>

relhandler()
{
	signal(SIGUSR1, relhandler);
	printf("relhandler called\n");
	ioctl(0, VT_RELDISP, VT_TRUE);
}

acqhandler()
{
	signal(SIGUSR2, acqhandler);
	printf("acqhandler called\n");
	ioctl(0, VT_RELDISP, VT_ACKACQ);
}

main(int argc, char **argv)
{
	struct vt_mode smode;
	int i;
	
	if (argc != 3) { printf("missing arg\n"); exit(); }
	smode.mode = atoi(argv[1]);
	smode.relsig = SIGUSR1;
	smode.acqsig = SIGUSR2;
	signal(SIGUSR1, relhandler);
	signal(SIGUSR2, acqhandler);
	ioctl(0, VT_SETMODE, &smode);
	for (i=0; i<atoi(argv[2]); i++) {
		sleep(5);
		ioctl(0, VT_GETMODE, &smode);
		printf("mode=%d, relsig=%d, acqsig=%d\n", 
			smode.mode, smode.relsig, smode.acqsig);
	}
}
