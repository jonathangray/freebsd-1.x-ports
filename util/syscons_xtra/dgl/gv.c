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
 *	Att: Soren Schmidt 	Email:	sos@kmd-ac.dk
 *	Tritonvej 36		UUCP:	...uunet!dkuug!kmd-ac!sos
 *	DK9210 Aalborg SO	Phone:  +45 9814 8076
 */

#include <stdio.h>
#include <string.h>
#include "dgl.h"
#include "prototypes.p"

typedef int boolean;
#define True (1)
#define False (0)

#define NEXTBYTE (*ptr++)
#define IMAGESEP 0x2c
#define INTERLACEMASK 0x40
#define COLORMAPMASK 0x80

int BitOffset,			/* Bit Offset of next code */
    XC, YC,			/* Output X and Y coords of current pixel */
    OutCount,			/* Decompressor output 'stack count' */
    Pass,			/* Used by output routine if interlaced pic */
    RWidth, RHeight,		/* screen dimensions */
    Width, Height,		/* image dimensions */
    LeftOfs, TopOfs,		/* image offset */
    BitsPerPixel,		/* Bits per pixel, read from GIF header */
    ColorMapSize,		/* number of colors */
    CodeSize,			/* Code size, read from GIF header */
    InitCodeSize,		/* Starting code size, used during Clear */
    Code,			/* Value returned by ReadCode */
    MaxCode,			/* limiting value for current code size */
    ClearCode,			/* GIF clear code */
    EOFCode,			/* GIF end-of-information code */
    CurCode, OldCode, InCode,	/* Decompressor variables */
    FirstFree,			/* First free code, generated per GIF spec */
    FreeCode,			/* Decompressor, next free slot in hash table */
    FinChar,			/* Decompressor variable */
    BitMask,			/* AND mask for data size */
    ReadMask;			/* Code AND mask for current code size */

boolean Interlace, HasColormap;

unsigned char *pixels;			/* The pixel array */

unsigned char *RawGIF;			/* The heap array to hold it, raw */
unsigned char *Raster;			/* The raster data stream, unblocked */

    /* The hash table used by the decompressor */
int Prefix[4096];
int Suffix[4096];

    /* An output array used by the decompressor */
int OutCode[1025];

    /* The color map, read from the GIF header */
unsigned char Red[256], Green[256], Blue[256];

char *id = "GIF87a";

int randmask[33] =	{
		-1,		-1,		0x3,		0x6,
		0xc,		0x14,		0x30,		0x60,
		0xb8,		0x110,		0x240,		0x500,
		0xca0,		0x1b00,		0x3500,		0x6000,
		0xb400,		0x12000,	0x20400,	0x72000,
		0x90000,	0x140000,	0x300000,	0x420000,
		0xd80000,	0x1200000,	0x3880000,	0x7200000,
		0x9000000,	0x14000000,	0x32800000,	0x48000000,
		0xa3000000
};

main(argc, argv)
int argc;
char *argv[];
{
int	interval=5, autoscale=0, slowfade=0, doublebuffer=0, center=0;
int	opt, i, Vgax, Vgay, init=0;
int	xscale, yscale, xs, ys;
char 	*tmppixels;
char 	buf[256];
extern	char *optarg;
extern  int  optind;

    while ((opt=getopt(argc, argv, "a:cdDi:s"))!=-1) {
	switch(opt) {
	    case 'a':
		autoscale=atoi(optarg)+1;
		break;
	    case 'c':
		center=1;
		break;
	    case 'D':
		doublebuffer=1;
		break;
	    case 'd':
		doublebuffer=2;
		break;
	    case 'i':
		interval=atoi(optarg);
		break;
	    case 's':
		slowfade=1;
		break;
	    default:
		usage();
        }
    }
    if (optind==argc) usage();
    if (slowfade && !autoscale) usage();
    if (doublebuffer && !autoscale) usage();
    if (autoscale>4 && doublebuffer) autoscale = 4;
    setbuf(stderr, NULL);
    for(; optind<argc; optind++) { 
    	if (access(argv[optind], 4)) {
	    if (!init) 
	        fprintf(stderr, "%s: %s not found\n", argv[0], argv[optind]);
	    else {
		sprintf(buf, "%s not found ... continuing", argv[optind]);
		sleep(interval);
	        DGLClear(DGLDisplay, darkest());
                DGLBitmapString(DGLDisplay, 0, DGLYsize-16, buf, brightest(), 0, 0 ,0);
	    }
	    continue;

	}
	timer(interval);
        if (decode_gif_file(argv[optind])) {
	    if (!init) 
	        fprintf(stderr, 
			"%s: something wrong with %s\n", argv[0], argv[optind]);
	    else {
	    char buf[128];
		sprintf(buf, 
			"something wrong with %s ... continuing", argv[optind]);
	        DGLClear(DGLDisplay, darkest());
                DGLBitmapString(DGLDisplay, 0, DGLYsize-16, buf, brightest(), 0, 0, 0);
	    }
	    continue;
	}
	DGLBeep(50,1000);
	if (init) { 
	    sleep(timer(0));
            if (!autoscale) {
	        if (Height<=400) DGLNewMode(CO256_640x400);
	        else if (Height<=480) DGLNewMode(CO256_640x480);
	        else if (Height<=600) DGLNewMode(CO256_800x600);
	        else if (Height<=768) DGLNewMode(CO256_1024x768);
	        else DGLNewMode(CO256_1152x900);
            }
	}
	else {
	    timer(0);
            if (!autoscale) {
	        if (Height<=400) DGLInitMode(CO256_640x400);
	        else if (Height<=480) DGLInitMode(CO256_640x480);
	        else if (Height<=600) DGLInitMode(CO256_800x600);
	        else if (Height<=768) DGLInitMode(CO256_1024x768);
	        else DGLInitMode(CO256_1152x900);
            }
	    else
	        DGLInitMode(autoscale-1);
            DGLSetPalette((char*)Red, (char*)Green, (char*)Blue);
	    DGLSetBorder(darkest());
	    DGLClear(DGLDisplay, darkest());
	}
/*
        if (autoscale && autoscale && !slowfade 
	    && doublebuffer && init) DGLBackBuffer();
*/
	if (!slowfade) {
	    DGLClear(DGLDisplay, darkest());
	    if (!doublebuffer) {
                DGLSetPalette((char*)Red, (char*)Green, (char*)Blue);
                DGLSetBorder(darkest());
	    }
	}
    	/* now display the picture through libdgl */
        if (!autoscale) {
            for (Vgay=0; Vgay<DGLYsize && Vgay<Height; Vgay++) 
                for (Vgax=0; Vgax<DGLXsize && Vgax<Width; Vgax++)
		    if (center) {
		        DGLSetXY(DGLDisplay, (Vgax+(DGLXsize-Width)/2), 
			           (Vgay+(DGLYsize-Height)/2),
			           pixels[Vgax+Vgay*Width]);
		    } else {
		        DGLSetXY(DGLDisplay, Vgax, Vgay, pixels[Vgax+Vgay*Width]);
		    }
        }
	else {
	    if (slowfade && init) {
		tmppixels = (char*) malloc(DGLXsize*DGLYsize);
		if (tmppixels == NULL) slowfade = 0;
		else memset(tmppixels, darkest(), DGLXsize*DGLYsize);
	    }
            yscale=(Height*100)/DGLYsize; xscale=(Width*100)/DGLXsize;
	    if (yscale>xscale) xscale=yscale; else yscale=xscale;
            for (Vgay=0; Vgay<DGLYsize; Vgay++) 
                for (Vgax=0; Vgax<DGLXsize; Vgax++) {
		    if ((xs=(Vgax * xscale)/100)>=Width) break;
		    if ((ys=(Vgay * yscale)/100)>=Height) break;
		    if (slowfade && init)
			tmppixels[Vgax+Vgay*DGLXsize]=pixels[xs+ys*Width];
		    else
			if (center) {
		            DGLSetXY(DGLDisplay, (Vgax+(DGLXsize-(Width*100/xscale))/2), 
		    	               (Vgay+(DGLYsize-(Height*100/yscale))/2),
			               pixels[xs+ys*Width]);
			} else {
		            DGLSetXY(DGLDisplay, Vgax, Vgay, pixels[xs+ys*Width]);
			}
	        }
	    if (slowfade && init) {
	        int mask;		/* mask to XOR w/ to create sequence */
	        unsigned int element;	/* one element of random sequence */
		unsigned char value, hits[256];
		memset(hits,'\0', 256);
	        value=darkest();
		DGLSetPaletteIndex(value, Red[value], Green[value], Blue[value]); 
	        DGLSetBorder(darkest());
	        mask = randmask[bitwidth((DGLXsize*DGLYsize)-1)];
	        element = 1;
	        do {
		    if(element < DGLXsize*DGLYsize) {
			hits[value=tmppixels[element]]++;
			if (hits[value] > 100) DGLSetPaletteIndex(value, 
			     Red[value], Green[value], Blue[value]); 
		        DGLSetPixel(DGLDisplay, element, value);
		    }
		    if(element & 1)
		        element = (element >> 1) ^ mask;
		    else
		        element >>= 1 ;
	        } while (element != 1);
        
                DGLSetPalette((char*)Red, (char*)Green, (char*)Blue);
		value=tmppixels[0];
	        DGLSetPixel(DGLDisplay, 0, value);
		free(tmppixels);
	    }
	    else if (doublebuffer && init) {
		if (doublebuffer==2) DGLBlankDisplay(1);
/*
		if (doublebuffer==1) DGLSwapBuffers(SWAP_SLOW);
		else DGLSwapBuffers(SWAP_FAST);
*/
                DGLSetPalette((char*)Red, (char*)Green, (char*)Blue);
	        DGLSetBorder(darkest());
		if (doublebuffer==2) DGLBlankDisplay(0);
	    }
	}
	sprintf(buf, "File: <%s>  Resolution: <%dx%d>",
		     argv[optind], DGLXsize, DGLYsize);
        DGLBitmapString(DGLDisplay, 0, DGLYsize-16, buf, brightest(), 0, 0, 0);
	free(pixels);
	init=1;
    }
    sleep(interval);
    DGLEnd();
    exit(0);
}

bitwidth(x)
{
	int b, w, i;

	i = w = 0;
	b = 1;
	for(i=0; i<32; i++)
	{	if(x & b)
			w = i;
		b <<= 1;
	}
	return(w+1);
}

usage()
{
    fprintf(stderr, 
"usage: gv [options] <giffile> ...\n"
"options:\n"
"          -a mode : autoscale to resolution mode, where mode is:\n"
"		     0=320x200, 1=640x400, 2=640x480, 3=800x600, 4=1024x768\n"
"          -c      : center all images\n"
"          -d      : double buffering mode, only works in autoscale mode 0-3\n"
"          -D      : double buffering mode, slow scroll between frames\n"
"          -i sec  : Wait minimum sec seconds between frames\n"
"          -s      : slow fade mode (experimental) only works in autoscale mode\n"
"\n"
"       GifViewer V1.1 Copyright (C) 1991 by DeepCore Technologies\n\n"
		    );
    exit(1);
}

timer(interval)
int interval;
{
static long val;
long now, res;
    now = time(NULL);
    res = val - now;
    val = now + interval;
    return res<0 ? 0 : res;
}

darkest()
{
unsigned char minred=63, mingreen=63, minblue=63;
int i, res;

    for (i=0; i<256; i++) {
	if (Red[i]<minred && Green[i]<mingreen && Blue[i]<minblue) {
	    minred=Red[i]; mingreen=Green[i]; minblue=Blue[i];
	    res=i;
	}
    }
    return res;
}

brightest()
{
unsigned char maxred=0, maxgreen=0, maxblue=0;
int i, res;
    
    for (i=0; i<256; i++) {
	if (Red[i]>maxred && Green[i]>maxgreen && Blue[i]>maxblue) {
	    maxred=Red[i]; maxgreen=Green[i]; maxblue=Blue[i];
	    res=i;
	}
    }
    return res;
}


decode_gif_file(fname)
char *fname;
{
int filesize;
FILE *fp;
register unsigned char ch, ch1;
register unsigned char *ptr, *ptr1;
register int i;

    fp = fopen(fname, "r");
    /* find the size of the file */
    fseek(fp, 0L, 2);
    filesize = ftell(fp);
    fseek(fp, 0L, 0);
    if (!(ptr = RawGIF = (unsigned char *) malloc(filesize)))
	return 1; /*fprintf(stderr, "not enough memory to read gif file\n");*/
    if (!(Raster = (unsigned char *) malloc(filesize)))
	return 1; /*fprintf(stderr, "not enough memory to read gif file\n");*/
    if (fread(ptr, filesize, 1, fp) != 1)
	return 1; /*fprintf(stderr, "GIF data read failed\n");*/
    if (strncmp((char*)ptr, id, 6))
	return 1; /*fprintf(stderr, "%s is not a GIF file\n", fname);*/
    ptr += 6;
    
    /* Get variables from the GIF screen descriptor */
    ch = NEXTBYTE;
    RWidth = ch + 0x100 * NEXTBYTE;	/* screen dimensions... not used. */
    ch = NEXTBYTE;
    RHeight = ch + 0x100 * NEXTBYTE;
    ch = NEXTBYTE;
    HasColormap = ((ch & COLORMAPMASK) ? True : False);
    BitsPerPixel = (ch & 7) + 1;
    ColorMapSize = 1 << BitsPerPixel;
    BitMask = ColorMapSize - 1;
    ch = NEXTBYTE;		/* background color... not used. */
    if (NEXTBYTE)		/* supposed to be NULL */
	return 1; /*fprintf(stderr,
			    "%s is a corrupt GIF file (nonull)\n", fname);*/

    /* Read in global colormap. */
    if (HasColormap) {
	for (i = 0; i < ColorMapSize; i++) {
	    Red[i] = (NEXTBYTE)>>2;
	    Green[i] = (NEXTBYTE)>>2;
	    Blue[i] = (NEXTBYTE)>>2;
	}

    }
    else {
        /* picture does not have a colormap - making one up */
	Red[0] = Green[0] = Blue[0] = 0;
	Red[1] = Green[1] = Blue[1] = 255;
    }

    /* Check for image seperator */
    if (NEXTBYTE != IMAGESEP)
	return 1; /*fprintf(stderr, 
			    "%s is a corrupt GIF file (nosep)\n", fname);*/

    /* Now read in values from the image descriptor */

    ch = NEXTBYTE;
    LeftOfs = ch + 0x100 * NEXTBYTE;
    ch = NEXTBYTE;
    TopOfs = ch + 0x100 * NEXTBYTE;
    ch = NEXTBYTE;
    Width = ch + 0x100 * NEXTBYTE;
    ch = NEXTBYTE;
    Height = ch + 0x100 * NEXTBYTE;
    Interlace = ((NEXTBYTE & INTERLACEMASK) ? True : False);


    /* Note that I ignore the possible existence of a local color map.
     * I'm told there aren't many files around that use them, and the spec
     * says it's defined for future use.  This could lead to an error
     * reading some files. 
     */
    /* Start reading the raster data. First we get the intial code size
     * and compute decompressor constant values, based on this code size.
     */
    CodeSize = NEXTBYTE;
    ClearCode = (1 << CodeSize);
    EOFCode = ClearCode + 1;
    FreeCode = FirstFree = ClearCode + 2;

    /* The GIF spec has it that the code size is the code size used to
     * compute the above values is the code size given in the file, but the
     * code size used in compression/decompression is the code size given in
     * the file plus one. (thus the ++).
     */
    CodeSize++;
    InitCodeSize = CodeSize;
    MaxCode = (1 << CodeSize);
    ReadMask = MaxCode - 1;

    /* Read the raster data.  Here we just transpose it from the GIF array
     * to the Raster array, turning it from a series of blocks into one long
     * data stream, which makes life much easier for ReadCode().
     */

    ptr1 = Raster;
    do {
	ch = ch1 = NEXTBYTE;
	while (ch--) *ptr1++ = NEXTBYTE;
	if ((ptr1 - Raster) > filesize)
	    return 1; /*fprintf(stderr, 
			"%s is a corrupt GIF file (unblock)\n", fname);*/
    } while(ch1);
    free(RawGIF);		/* We're done with the raw data now... */

    /* Allocate the pixel array. */
    pixels = (unsigned char*)malloc(Width*Height);

    /* Zero out some globals */
    BitOffset = Pass = OutCount = XC = YC = 0;

    /* Decompress the file, continuing until you see the GIF EOF code.
     * One obvious enhancement is to add checking for corrupt files here.
     */
    Code = ReadCode();
    while (Code != EOFCode) {

    /* Clear code sets everything back to its initial value, then reads the
     * immediately subsequent code as uncompressed data.
     */

	if (Code == ClearCode) {
	    CodeSize = InitCodeSize;
	    MaxCode = (1 << CodeSize);
	    ReadMask = MaxCode - 1;
	    FreeCode = FirstFree;
	    CurCode = OldCode = Code = ReadCode();
	    FinChar = CurCode & BitMask;
	    AddToPixel(FinChar);
	}
	else {

    /* If not a clear code, then must be data: 
     * save same as CurCode and InCode 
     */
	    CurCode = InCode = Code;

    /* If greater or equal to FreeCode, not in the hash table yet;
     * repeat the last character decoded
     */
	    if (CurCode >= FreeCode) {
		CurCode = OldCode;
		OutCode[OutCount++] = FinChar;
	    }

    /* Unless this code is raw data, pursue the chain pointed to by CurCode
     * through the hash table to its end; each code in the chain puts its
     * associated output code on the output queue.
     */
	    while (CurCode > BitMask) {
		if (OutCount > 1024)
		    return 1; /*fprintf(stderr,
			"%s is a corrupt GIF file (OutCount)\n", fname);*/
		OutCode[OutCount++] = Suffix[CurCode];
		CurCode = Prefix[CurCode];
	    }

    /* The last code in the chain is treated as raw data. */
	    FinChar = CurCode & BitMask;
	    OutCode[OutCount++] = FinChar;

    /* Now we put the data out to the Output routine.
     * It's been stacked LIFO, so deal with it that way...
     */
	    for (i = OutCount - 1; i >= 0; i--)
		AddToPixel(OutCode[i]);
	    OutCount = 0;

    /* Build the hash table on-the-fly. No table is stored in the file. */
	    Prefix[FreeCode] = OldCode;
	    Suffix[FreeCode] = FinChar;
	    OldCode = InCode;

    /* Point to the next slot in the table.  If we exceed the current
     * MaxCode value, increment the code size unless it's already 12.  If it
     * is, do nothing: the next code decompressed better be CLEAR
     */
	    FreeCode++;
	    if (FreeCode >= MaxCode) {
		if (CodeSize < 12) {
		    CodeSize++;
		    MaxCode *= 2;
		    ReadMask = (1 << CodeSize) - 1;
		}
	    }
	}
	Code = ReadCode();
    }
    free(Raster);		/* No use for Raster any more */
    fclose(fp);
    return 0;
}

/* Fetch the next code from the raster data stream.  The codes can be
 * any length from 3 to 12 bits, packed into 8-bit bytes, so we have to
 * maintain our location in the Raster array as a BIT Offset.  We compute
 * the byte Offset into the raster array by dividing this by 8, pick up
 * three bytes, compute the bit Offset into our 24-bit chunk, shift to
 * bring the desired code to the bottom, then mask it off and return it. 
 */
ReadCode()
{
int RawCode, ByteOffset;

    ByteOffset = BitOffset / 8;
    RawCode = Raster[ByteOffset] + (0x100 * Raster[ByteOffset + 1]);
    if (CodeSize >= 8)
	RawCode += (0x10000 * Raster[ByteOffset + 2]);
    RawCode >>= (BitOffset % 8);
    BitOffset += CodeSize;
    return(RawCode & ReadMask);
}


AddToPixel(Index) /* Store a pixel. */
unsigned char Index;
{
    if (YC < Height)
	pixels[XC+YC*Width]=Index;

    /* Update the X-coordinate, and if it overflows, update the Y-coordinate */
    if (++XC == Width) {

    /* If a non-interlaced picture, just increment YC to the next scan line. 
     * If it's interlaced, deal with the interlace as described in the GIF
     * spec.  Put the decoded scan line out to the screen if we haven't gone
     * past the bottom of it
     */
	XC = 0;
	if (!Interlace) YC++;
	else {
	    switch (Pass) {
		case 0:
		    YC += 8;
		    if (YC >= Height) {
			Pass++;
			YC = 4;
		    }
		break;
		case 1:
		    YC += 8;
		    if (YC >= Height) {
			Pass++;
			YC = 2;
		    }
		break;
		case 2:
		    YC += 4;
		    if (YC >= Height) {
			Pass++;
			YC = 1;
		    }
		break;
		case 3:
		    YC += 2;
		break;
	    }
	}
    }
}
