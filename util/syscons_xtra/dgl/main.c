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
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/signal.h>
#include <machine/console.h>
#include "dgl.h"
#include "driver.h"

#define SIG_REL	SIGUSR1
#define SIG_ACQ	SIGUSR2

/*
 * local function declarations.
 */
static void DGLSwitchAway(), DGLSwitchBack(), DGLAbort();

/*
 * internal only symbols.
 */
static int DGLOldMode;
static int DGLOnDisplay;

/*
 * external used symbols.
 */
DGLBitmap *DGLDisplay;
int 	  DGLXsize;
int 	  DGLYsize;
int	  DGLSwitchPending;

/*
 * Initially set up the graphics multiscreen stuff and set up video card
 * in the specified mode.
 */
DGLInitMode(mode)
int mode;
{
struct vt_mode smode;


  /*
   *  Set up to catch the screen switch signals.
   */
  signal(SIG_REL, DGLSwitchAway);
  signal(SIG_ACQ, DGLSwitchBack);
  signal(SIGINT, DGLAbort);
  DGLOnDisplay = 1;
  DGLSwitchPending = 0;

  /*
   * Get priviledge to do direct INs and OUTs to the video card.
   */
  ioctl(0, KDENABIO, 0);

  /*
   * Wait for this virtual console to become active 
   */
  ioctl(0, VT_WAITACTIVE, 0);

  ioctl(0, KDSETMODE, KD_GRAPHICS);

  if (SvgaMode(mode, 1)) {
    ioctl(0, KDSETMODE, KD_TEXT);
    ioctl(0, KDDISABIO, 0);
    return 1;
  }
  /*
   * Setup environment 
   */
  DGLDisplay = (DGLBitmap*) malloc(sizeof(DGLBitmap));
  if (DGLDisplay == (DGLBitmap*)0) {
    DGLEnd();
    return 1;
  }
  DGLDisplay->Xsize = DGLXsize;
  DGLDisplay->Ysize = DGLYsize;
  DGLDisplay->Type = VIDBUF;
  DGLDisplay->Bitmap = SvgaMem;

  /*
   * Set up the data structure that asks the driver
   * to send signals when the screens are switched.
   * mode == VT_PROCESS means send screen switch signals.
   * mode == VT_AUTO means turn off screen switch signals (regular mode).
   * relsig == the signal we want when the user switches away.
   * acqsig == the signal we want when the user switches back to us.
   */
  smode.mode = VT_PROCESS;
  smode.waitv = 0;	/* not implemented, reserved */
  smode.relsig = SIG_REL;
  smode.acqsig = SIG_ACQ;
  smode.frsig  = SIGINT;	
  if (ioctl(0, VT_SETMODE, &smode) == -1) {
    DGLEnd();
    return 1;
  }

  DGLTextSetFontFile((char*)0);
  return 0;
}

/*
 * Change graphics mode to the specified mode.
 */
DGLNewMode(mode)
int mode;
{
  while (!DGLOnDisplay) sleep(1); 
  if (DGLSwitchPending) 
    DGLSwitchAllow();
  if (SvgaMode(mode, 0)) 
    return 1;
  DGLDisplay->Xsize = DGLXsize;
  DGLDisplay->Ysize = DGLYsize;
}

/*
 * restore text mode.
 */
void
DGLEnd()
{
struct vt_mode smode;

  while (!DGLOnDisplay) sleep(1); 
  if (DGLSwitchPending) 
    DGLSwitchAllow();
  SvgaExit();
  ioctl(0, KDSETMODE, KD_TEXT);
  ioctl(0, KDDISABIO, 0);
  smode.mode = VT_AUTO;
  ioctl(0, VT_SETMODE, &smode);
  free(DGLDisplay);
}

/*
 * this is the signal handler for when the user screen flips
 * away from us.
 */
static void DGLSwitchAway()
{
  DGLOnDisplay = 0;
  DGLSwitchPending = 1;
  signal(SIG_REL, DGLSwitchAway);
}

/*
 * this is the signal handler for when the user screen flips
 * back to us.
 */
static void DGLSwitchBack()
{
  DGLOnDisplay = 1;
  DGLSwitchPending = 1;
  signal(SIG_ACQ, DGLSwitchBack);
}

DGLSwitchAllow()
{
  DGLSwitchPending = 0;
  if (DGLOnDisplay) {
    ioctl(0, KDENABIO, 0);
    ioctl(0, KDSETMODE, KD_GRAPHICS);
    SvgaRestore();
    /*
     * Tell the video driver that we have restored our state
     * and screen switching can now continue.
     */
    ioctl(0, VT_RELDISP, VT_ACKACQ);
    DGLDisplay->Type = VIDBUF;
    DGLDisplay->Bitmap = SvgaMem;
  }
  else {
    SvgaSave();
    ioctl(0, KDDISABIO, 0);
    ioctl(0, KDSETMODE, KD_TEXT);
    /*
     * Tell the video driver that we have saved our state
     * and it can now have the card to switch to the new screen.
     * The video driver waits (forever) for this ioctl before
     * it will complete the screen switch requested by the user.
     */
    ioctl(0, VT_RELDISP, VT_TRUE);
    DGLDisplay->Type = MEMBUF;
    DGLDisplay->Bitmap = SvgaBuf;
  }
  return 0;
}

static void DGLAbort()
{
  DGLEnd();
  exit(0);
}

