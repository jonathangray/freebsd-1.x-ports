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

#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/malloc.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/console.h>
#include "dgl.h"
#include "stdpalette.h"
#include "driver.h"
#include "modetables.h"

/*
 * local variables
 */
static SvgaRegs *SvgaOriginalMode=(SvgaRegs*)0;	/* save org mode here */
static int Xtals[16] = {25,28,33,36, 40,45,32,38, 50,57,65,72, 80,90,63,75};

/*
 * global variables 
 */
SvgaRegs        *SvgaGraphicsMode=NULL;	/* our mode saved here */ 
char     	*SvgaMem;		/* video memory  ptr */
char            *SvgaBuf=(char*)0;	/* save area for screen */
int		SvgaSaveSeg;		/* saved segment offset */

/*
 * some handy defines 
 */
#define		B64K		(65536)
#define		B256K		(4*B64K)

SvgaMode(mode, init)
int mode, init;
{
int i;

  if ((SvgaGraphicsMode=SvgaSetupRegs(&SvgaModeTab[mode], SvgaGraphicsMode)) == NULL)
    return 1;
  DGLXsize=(SvgaModeTab[mode].Flags & (SV_HALFCLOCK | SV_HICOLOR)
    ? SvgaModeTab[mode].HDisplay/2 : SvgaModeTab[mode].HDisplay);
  DGLYsize=(SvgaModeTab[mode].Flags & SV_HALFCLOCK 
    ? SvgaModeTab[mode].VDisplay/2 : SvgaModeTab[mode].VDisplay);

  if (init) {
    SvgaOriginalMode=SvgaGetmode(SvgaOriginalMode);
    for (i=0x00; i<0x100; i++) {
      SvgaGraphicsMode->LUT[i*3+0] = palette_red[i];
      SvgaGraphicsMode->LUT[i*3+1] = palette_grn[i];
      SvgaGraphicsMode->LUT[i*3+2] = palette_blu[i];
    }
    SvgaMem = (char*)mmap(0, 0x20000, PROT_READ|PROT_WRITE, MAP_FILE, 
		          open(ttyname(0), O_RDWR), 0);
    if (SvgaMem <= (char*)0)
      return 1;
    for (i=0; i<16; i++) {
      DGLSetSegment(i);
      memset(SvgaMem, 0x00, B64K);
    }
  }
  if (SvgaBuf)
    free(SvgaBuf);
  SvgaBuf = (char*)malloc((((DGLXsize*DGLYsize)>>16)+1)<<16);
  if (SvgaBuf == NULL)
    return 1;
  SvgaSetmode(SvgaGraphicsMode);
  DGLSetSegment(0);
  return 0;
}

void SvgaExit()
{
  SvgaSetmode(SvgaOriginalMode);
  free(SvgaOriginalMode);
  free(SvgaGraphicsMode);
  free(SvgaBuf);
  DGLSetSegment(0);
}

void SvgaSave()
{
int i;

  for (i=0; i<=(DGLXsize*DGLYsize)>>16; i++) {
    outb(0x3CD,i<<4);
    memcpy(SvgaBuf+(i*B64K), SvgaMem, B64K);
  }
  SvgaSetmode(SvgaOriginalMode);
  DGLSetSegment(0);
} 

void SvgaRestore()
{
int i;

  SvgaSetmode(SvgaGraphicsMode);
  for (i=0; i<=(DGLXsize*DGLYsize)>>16; i++) {
    outb(0x3CD,i);
    memcpy(SvgaMem, SvgaBuf+(i*B64K), B64K);
  }
  DGLSetSegment(SvgaSaveSeg);
}

SvgaRegs *SvgaSetupRegs(setup, mode)
SvgaSetup *setup;
SvgaRegs *mode;
{
int xtalno;

  if ((xtalno = SvgaFindXtalno(setup->Clock)) < 0) {
    if (mode) free(mode);
    return NULL;
  }

  if (mode == (SvgaRegs*)NULL) 
    mode = (SvgaRegs*)malloc(sizeof(SvgaRegs));
  
  /*
   * Time Sequencer
   */
  mode->TS[0x00] = 0x01;
  mode->TS[0x01] = 0x01;
  mode->TS[0x02] = 0x0F;
  mode->TS[0x03] = 0x00;
  mode->TS[0x04] = (setup->Flags & (SV_256COLOR | SV_HICOLOR)) ? 0x0E : 0x06; 
  mode->TS[0x06] = 0x00; 
  /*mode->TS[0x07] = ((0x40 ^ ((xtalno & 0x08) << 3)) | 0xAC);*/
  mode->TS[0x07] = 0xEC;
  /*
   * compute correct Hsync & Vsync polarity 
   */
  if ((setup->Flags & (SV_PHSYNC | SV_NHSYNC))
      && (setup->Flags & (SV_PVSYNC | SV_NVSYNC))) {
    mode->MISC = 0x23;
    if (setup->Flags & SV_PHSYNC) mode->MISC |= 0x40;
    if (setup->Flags & SV_PVSYNC) mode->MISC |= 0x80;
  }
  else {
    if (setup->VDisplay < 400) mode->MISC = 0xA3;
    else if (setup->VDisplay < 480) mode->MISC = 0x63;
    else if (setup->VDisplay < 768) mode->MISC = 0xE3;
    else mode->MISC = 0x23;
  mode->MISC |= ((xtalno & 0x03) << 2);
  }
  /*
   * CRTC Controller
   */
  mode->CRTC[0x00] = (setup->HTotal >> 3) - 5;  
  mode->CRTC[0x01] = (setup->HDisplay >> 3) - 1;
  mode->CRTC[0x02] = (setup->HDisplay >> 3) + 1;
  mode->CRTC[0x03] = (((setup->HTotal >> 3) - 6) & 0x1F) | 0x80;
  mode->CRTC[0x04] = (setup->HSyncStart >> 3);
  mode->CRTC[0x05] = ((((setup->HTotal >> 3) -6 ) & 0x20 ) << 2 )
                     | (((setup->HSyncEnd >> 3)) & 0x1F);
  mode->CRTC[0x06] = (setup->VTotal - 2) & 0xFF;
  mode->CRTC[0x07] = (((setup->VTotal - 2) & 0x100) >> 8 )
                     | (((setup->VDisplay - 1) & 0x100) >> 7 )
                     | (((setup->VSyncStart) & 0x100) >> 6 )
	             | (((setup->VSyncStart) & 0x100) >> 5 ) 
	             | 0x10
	             | (((setup->VTotal - 2) & 0x200) >> 4 )
	             | (((setup->VDisplay - 1) & 0x200) >> 3 )
		     | (((setup->VDisplay) & 0x200) >> 2 );
  mode->CRTC[0x08] = 0x00;
  mode->CRTC[0x09] = (((setup->VSyncStart) & 0x200) >> 4 )  /**/
		     | ((setup->Flags & SV_HALFCLOCK) ? 0x41 : 0x40);
  mode->CRTC[0x0A] = 0x00;
  mode->CRTC[0x0B] = 0x00;
  mode->CRTC[0x0C] = 0x00;
  mode->CRTC[0x0D] = 0x00;
  mode->CRTC[0x0E] = 0x00;
  mode->CRTC[0x0F] = 0x00;
  mode->CRTC[0x10] = setup->VSyncStart & 0xFF;
  mode->CRTC[0x11] = (setup->VSyncEnd & 0x0F) | 0x20;
  mode->CRTC[0x12] = (setup->VDisplay - 1) & 0xFF;
  mode->CRTC[0x13] = (setup->Flags & (SV_256COLOR | SV_HICOLOR)) ?
		       ((setup->Flags & SV_HALFCLOCK) ? 
		     	 (setup->HDisplay>>4) : (setup->HDisplay>>3)) 
		     :
		       ((setup->Flags & SV_HALFCLOCK) ? 
		     	 (setup->HDisplay>>5) : (setup->HDisplay>>4));
  mode->CRTC[0x14] = (setup->Flags & (SV_256COLOR | SV_HICOLOR)) ?
                     ((setup->Flags & SV_HALFCLOCK) ? 0x40 : 0x60) : 0x00;
  mode->CRTC[0x15] = (setup->VDisplay + 1) & 0xFF; /*SyncStart*/
  mode->CRTC[0x16] = (setup->VSyncStart + 1) & 0xFF;
  mode->CRTC[0x17] = ((setup->Flags & (SV_256COLOR | SV_HICOLOR)) ? 
		     ((setup->Flags & SV_HALFCLOCK) ? 0xA3 : 0xAB) : 0xC3);
  mode->CRTC[0x18] = 0xFF;
  mode->CRTC[0x19] = 0x00;
  mode->CRTC[0x1A] = ((xtalno & 0x04) >> 1) | 0x08;
  mode->CRTC[0x1B] = ((setup->Flags & SV_INTERLACE) ? 0x80 : 0x00)
                     | 0x10
                     | ((setup->VSyncStart & 0x400) >> 7 )
	             | (((setup->VDisplay) & 0x400) >> 8 )
	             | (((setup->VTotal -2) & 0x400) >> 9 )
	             | (((setup->VSyncStart) & 0x400) >> 10 );
  /*
   * Attribute Controller
   */
  mode->ATC[0x00] = 0x00;	
  mode->ATC[0x01] = 0x01;
  mode->ATC[0x02] = 0x02;
  mode->ATC[0x03] = 0x03;
  mode->ATC[0x04] = 0x04;
  mode->ATC[0x05] = 0x05;
  mode->ATC[0x06] = 0x06;
  mode->ATC[0x07] = 0x07;
  mode->ATC[0x08] = 0x08;
  mode->ATC[0x09] = 0x09;
  mode->ATC[0x0A] = 0x0A;
  mode->ATC[0x0B] = 0x0B;
  mode->ATC[0x0C] = 0x0C;
  mode->ATC[0x0D] = 0x0D;
  mode->ATC[0x0E] = 0x0E;
  mode->ATC[0x0F] = 0x0F;
  mode->ATC[0x10] = (setup->Flags & SV_HALFCLOCK) ? 0x41 : 0x01;
  mode->ATC[0x11] = 0x00;	/* Border color */
  mode->ATC[0x12] = 0x0F;
  mode->ATC[0x13] = 0x00;
  mode->ATC[0x14] = 0x00;
  mode->ATC[0x15] = 0x00;
  mode->ATC[0x16] = ((setup->Clock > 45 && !(setup->Flags & SV_INTERLACE)) ? 0x10 : 0x00) 
		    | ((setup->Flags & SV_256COLOR) ? 0x80 : 0x00)
		    | ((setup->Flags & SV_HICOLOR) ? 0xB0 : 0x00);
  /*
   * Graphics Display Controller
   */
  mode->GDC[0x00] = 0x00;
  mode->GDC[0x01] = 0x00;
  mode->GDC[0x02] = 0x00;
  mode->GDC[0x03] = 0x00;
  mode->GDC[0x04] = 0x00;
  mode->GDC[0x05] = (setup->Flags & (SV_256COLOR | SV_HICOLOR)) ? 0x40 : 0x00;
  mode->GDC[0x06] = (setup->Flags & (SV_256COLOR | SV_HICOLOR)) ? 0x05 : 0x01;
  mode->GDC[0x07] = 0x0F;
  mode->GDC[0x08] = 0xFF;
  return mode;
}

SvgaFindXtalno(int clock)
{
int i;
  for (i=0; i<16; i++)
    if (Xtals[i] == clock) return i;	
  return -1;
}

SvgaSetmode(svgatab)
SvgaRegs *svgatab;
{
int i;

  outb(0x3C4, 0x00); outb(0x3C5, 0x00);	/* stop sequencer */
  outb(0x3BF, 0x03); outb(0x3D8, 0xA0);	/* unlock ET4000 ext. regs */
  
  for (i=0x00; i<0x08;  i++) {		/* setup Timing Sequencer */ 
    if (i == 0x05) 
      continue;
    outb(0x3C4, i); outb(0x3C5, svgatab->TS[i]);
  }

  outb(0x3C2, svgatab->MISC);		/* setup Misc Output Register */
  outb(0x3C4, 0x00); outb(0x3C5, 0x03);	/* start sequencer */
  outb(0x3D4, 0x11); outb(0x3D5, 0x00);	/* unlock CRTC registers 0-7 */

  for (i=0x00; i<0x19; i++) {		/* setup CRT Controller */
    outb(0x3D4, i); outb(0x3D5, svgatab->CRTC[i]);
  } 
  outb(0x3D4, 0x33); outb(0x3D5, svgatab->CRTC[i++]);
  outb(0x3D4, 0x34); outb(0x3D5, svgatab->CRTC[i++]);
  outb(0x3D4, 0x35); outb(0x3D5, svgatab->CRTC[i]);

  /* program Attribute Controller */
  inb(0x3DA); 				/* reset flip-flop */
  for (i=0x00; i<0x17; i++) { 
    outb(0x3C0,i); outb(0x3C0, svgatab->ATC[i]); 
  }
  
  for (i=0x00; i<0x09;  i++) {		/* setup Graphics Data Controller */
    outb(0x3CE, i); outb(0x3CF, svgatab->GDC[i]);
  }
 
  outb(0x3C6, 0xFF);			/* no pixel mask */

  /* is it a HiColor(tm) mode */
  if ((svgatab->ATC[0x16] & 0xB0) == 0xB0) {
    inb(0x3C6); inb(0x3C6);		/* four reads to pixel mask selects */
    inb(0x3C6); inb(0x3C6); 		/* the Sierra DAC's mode register */
    outb(0x3C6, 0x80);			/* select HiColor(tm) mode */
  }
  else{
    inb(0x3C6); inb(0x3C6); 		/* four reads to pixel mask selects */
    inb(0x3C6); inb(0x3C6); 		/* the Sierra DAC's mode register */
    outb(0x3C6, 0x00);			/* normal DAC mode */
  }

  outb(0x3C8, 0x00);
  for (i=0x00; i<0x300; i++) {		/* setup Palette registers */
    outb(0x3C9, svgatab->LUT[i]);
  }

  inb(0x3DA); 				/* reset flip-flop */
  outb(0x3BF, 0x01); outb(0x3D8, 0xA0);	/* lock ET4000 ext. regs */
  outb(0x3C0, 0x20);			/* enable Palette */
}

SvgaRegs *SvgaGetmode(svgatab)
SvgaRegs *svgatab;
{
int i;

  if (svgatab == (SvgaRegs*)NULL) 
    svgatab = (SvgaRegs*)malloc(sizeof(SvgaRegs));

  svgatab->MISC = inb(0x3CC);

  for (i=0; i<0x08;  i++) {
    outb(0x3C4,i); svgatab->TS[i] = inb(0x3C5); 
  }
  svgatab->TS[0x07] |= 0xA8;		/* set 32Kbyte BIOS size */

  for (i=0x00; i<0x19; i++) { 
    outb(0x3D4,i); svgatab->CRTC[i] = inb(0x3D5); 
  }
  outb(0x3D4, 0x33); svgatab->CRTC[i++] = inb(0x3D5); 
  outb(0x3D4, 0x34); svgatab->CRTC[i++] = inb(0x3D5);  
  outb(0x3D4, 0x35); svgatab->CRTC[i] = inb(0x3D5); 

  svgatab->CRTC[0x11] &= 0x7F;  	/* clear write protect bit */

  for (i=0x00; i<0x09; i++) { 
    outb(0x3CE,i); svgatab->GDC[i] = inb(0x3CF);
  }

  inb(0x3DA); 				/* reset flip-flop */
  for (i=0; i<0x17; i++) { 
    outb(0x3C0,i); svgatab->ATC[i] = inb(0x3C1);
    outb(0x3C0, svgatab->ATC[i]); 
  }

  outb(0x3C7, 0x00);	
  for (i=0x00; i<0x300; i++)            /* get palette entries */
    svgatab->LUT[i] = inb(0x3C9);
  return svgatab;
}

SvgaDebug(regtab)
SvgaRegs *regtab;
{
int i;

  fprintf(stdout,"\nMiscOut 0x%02x\n", regtab->MISC);
  fprintf(stdout,"\nCRTC registers\n");
  for (i=0; i<0x19; i++) {
    fprintf(stdout," id=0x%02x = 0x%02x", i, regtab->CRTC[i]);
    if (i%4==3) fprintf(stdout,"\n"); else fprintf(stdout, ", ");
  }
  fprintf(stdout,"\n id=0x33 = 0x%02x, ", regtab->CRTC[i++]);
  fprintf(stdout," id=0x34 = 0x%02x, ", regtab->CRTC[i++]);
  fprintf(stdout," id=0x35 = 0x%02x\n", regtab->CRTC[i]);
  fprintf(stdout,"TS registers\n");
  for (i=0; i<0x08; i++) {
    fprintf(stdout," id=0x%02x = 0x%02x", i, regtab->TS[i]);
    if (i%4==3) fprintf(stdout,"\n"); else fprintf(stdout, ", ");
  }
  fprintf(stdout,"GDC registers\n");
  for (i=0; i<0x09; i++) {
    fprintf(stdout," id=0x%02x = 0x%02x", i, regtab->GDC[i]);
    if (i%4==3) fprintf(stdout,"\n"); else fprintf(stdout, ", ");
  }
  fprintf(stdout,"\nATC registers\n");
  for (i=0; i<0x17; i++) {
    fprintf(stdout," id=0x%02x = 0x%02x", i, regtab->ATC[i]);
    if (i%4==3) fprintf(stdout,"\n"); else fprintf(stdout, ", ");
  }
} 

