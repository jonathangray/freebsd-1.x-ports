/*
 *  Copyright (C) 1991 By DeepCore Technologies
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 1, or any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *      DeepCore Technologies
 *	Att: Søren Schmidt 	Email:	sos@kmd-ac.dk
 *	Tritonvej 36		UUCP:	...uunet!dkuug!kmd-ac!sos
 *	DK9210 Aalborg SO	Phone:  +45 9814 8076
 */

/*
 * definition of possible screen formats
 */
SvgaSetup SvgaModeTab[30] = {
	SV_256COLOR | SV_HALFCLOCK, 25,
	640, 800, 680, 768, 
	400, 447, 412, 414, 

	SV_256COLOR, 25,
	640, 800, 672, 768, 
	400, 447, 412, 414,

	SV_256COLOR, 25,
	640, 800, 672, 768, 
	480, 525, 490, 492,

	SV_256COLOR, 50,
	800, 1056, 840, 968,
	600, 628, 601, 605,

	SV_256COLOR, 65,
	1024, 1344, 1032, 1176,
	768, 806, 771, 777,
	
	SV_256COLOR, 75,
	1024, 1328, 1048, 1184,
	768, 806, 771, 777,

	SV_256COLOR, 80,
	1152, 1440, 1200, 1280,
	900, 930, 902, 904,

	0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,

	SV_HICOLOR | SV_HALFCLOCK, 25,
	640, 800, 680, 768, 
	400, 447, 412, 414, 

	SV_HICOLOR, 25,
	640, 800, 672, 768, 
	400, 447, 412, 414,

	SV_HICOLOR, 25,
	640, 800, 672, 768, 
	480, 525, 490, 492,

	SV_HICOLOR, 80,
	/*800, 1056, 840, 968,*/
	1600, 1880, 1680, 1740,
	600, 618, 601, 605,

	SV_HICOLOR, 65,
	1024, 1344, 1032, 1176,
	768, 806, 771, 777,
	
	SV_HICOLOR, 75,
	1024, 1328, 1048, 1184,
	768, 806, 771, 777,

	SV_HICOLOR, 80,
	1152, 1440, 1200, 1280,
	900, 930, 902, 904,

	0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,
	SV_HALFCLOCK, 25,
	640, 800, 680, 768, 
	400, 447, 412, 414, 

	0, 25,
	640, 800, 672, 768, 
	400, 447, 412, 414,

	0, 25,
	640, 800, 672, 768, 
	480, 525, 490, 492,

	0, 40,
	800, 1056, 840, 968,
	600, 628, 601, 605,

	0, 65,
	1024, 1344, 1032, 1152,
	768, 806, 771, 777,
	
	0, 75,
	1024, 1328, 1048, 1184,
	768, 806, 771, 777,

	0, 80,
	1152, 1440, 1200, 1280,
	900, 930, 902, 904,

	0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,
};

