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
#include <sys/console.h>

extern int	getopt();
extern char*	optarg;
extern int	opterr;

#define DELAYS		4	/* number of delays          */
#define RATES		32	/* number of rates           */
#define DELAY_SHIFT	5	/* delay pos control byte    */
#define RATE_SHIFT	0	/* rate pos in control byte  */

static int delays[DELAYS] = {250, 500, 750, 1000};
static int rates[RATES] = { 34,  38,  42,  46,  50,  55,  59,  63,
			    68,  76,  84,  92, 100, 110, 118, 126,
			   136, 152, 168, 184, 200, 220, 236, 252,
			   272, 304, 336, 368, 400, 440, 472, 504};

main(argc, argv)
int argc;
char **argv;
{
  int delay = 0;		/* default delay short */
  int rate = 0;			/* default rate fast */
  int verbose = 0;		/* default quiet */
  int flag;
  int n;

  opterr = 0;
  while ((flag = getopt(argc, argv, "d:r:v")) != -1)
    switch(flag) {
    case 'd':
    	n = atoi(optarg);
    	for (delay = 0; delay < DELAYS-1 && n > delays[delay]; delay++) ;
    	break;
    case 'r':
    	n = atoi(optarg);
    	for (rate = 0; rate < RATES-1 && n > rates[rate]; rate++) ;
    	break;
    case 'v':
    	verbose = 1;
    	break;
    default:
    	printf("Usage: %s [-d delay] [-r rate] [-v]\n", argv[0]);
    	exit(1);
    }
  n = (delay << DELAY_SHIFT) | (rate << RATE_SHIFT);
  ioctl(0, KDSETRAD, n);
  if (verbose)
    printf("%s: %d ms repeat delay and %d ms repeat rate.\n",
           argv[0], n, delays[delay], rates[rate]);
}
