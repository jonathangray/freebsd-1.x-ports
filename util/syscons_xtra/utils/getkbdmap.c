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

keymap_t map;

main()
{
int i,j;
	ioctl(0, GIO_KEYMAP, &map);
	printf("keymap_t keymap = { %d,\n", map.n_keys);
printf(
"/*                                                            alt"
"\n"
" * scan                          cntrl          alt    alt   cntrl"
"\n"
" * code     base   shift  cntrl  shift   alt   shift  cntrl  shift  spcl  flgs"
"\n"
" * ---------------------------------------------------------------------------"
"\n"
" */\n"
);
	for (i=0; i<map.n_keys; i++) {

		printf("/* sc=%02x */ ", i, i);
		for (j=0; j<8; j++) {
			spec_key(map.key[i].map[j], (0x80>>j)&map.key[i].spcl);
		}
		printf("0x%02X, 0x%02X,\n", map.key[i].spcl, map.key[i].flgs);
	}
	printf("};\n");
}

spec_key(key, spc)
unsigned char key, spc;
{
	if (spc) 
		switch (key) {
		case 0:   printf(" NOP,  "); break;	
		case 2:   printf(" LSH,  "); break;
		case 3:   printf(" RSH,  "); break;
		case 4:   printf(" CLK,  "); break;
		case 5:   printf(" NLK,  "); break;
		case 6:   printf(" SLK,  "); break;
		case 7:   printf("LALT,  "); break;
		case 124: printf("RALT,  "); break;
		case 9:   printf("LCTR,  "); break;
		case 123: printf("RCTR,  "); break;
		default:
			if (key>=27 && key<=122) {
		 		printf("F(%2d), ", key - 26);
				break;
			}
			if (key>=11 && key<=26) {
	 			printf("S(%2d), ", key - 10);
				break;
			}
			printf("0x%02X,  ", key);
		}
	else 
		printchar(key);
}

printchar(chr)
unsigned char chr;
{
	switch (chr) {

	case '\'': printf(" \'\\\'\', "); break;

	case '\\': printf(" \'\\\\\', "); break;

	default:
		if (chr>=' ' && chr<127) printf(" '%c',  ", chr); 
		else printf("0x%02X,  ", chr);
		break;
	}
}
