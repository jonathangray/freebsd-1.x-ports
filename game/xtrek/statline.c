/*
 * Copyright 1989 Jon Bennett, Mike Bolotski, David Gagne, Dan Lovinger
 * Copyright 1986 Chris Gutherie
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of the copyright holders not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.  The copyright holders make no
 * representations about the suitability of this software for any purpose.
 * It is provided "as is" without express or implied warranty.
 *
 * THE COPYRIGHT HOLDERS DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 */

#include <X11/Xlib.h>

#include <stdio.h>
#include <signal.h>
#include <math.h>

#include "defs.h"
#include "data.h"

#define PRINT3(num,buf,s) { \
   int __lpr = num; \
   if (__lpr <= 0) \
      buf[(s)] =  buf[(s)+1] =  buf[(s)+2] = ' '; \
   else if (__lpr < 10) { \
      buf[(s)] =  buf[(s)+1] = ' '; \
      buf[(s)+2] = __lpr + '0'; \
    } \
    else if (__lpr < 100) { \
      buf[(s)] = ' '; \
      buf[(s)+1] = __lpr / 10 + '0'; \
      buf[(s)+2] = __lpr % 10 + '0'; \
    } \
    else { \
      buf[(s)] = __lpr / 100 + '0'; \
      buf[(s)+1] = __lpr / 10 % 10 + '0'; \
      buf[(s)+2] = __lpr % 10 + '0'; \
    }  \
}

#define PRINT2(num, buf, s)  { \
   int __lpr = num;\
   if (__lpr <= 0) \
      buf[(s)] =  buf[(s)+1] = ' '; \
    else if (__lpr < 10) { \
      buf[(s)] = ' '; \
      buf[(s)+1] = __lpr + '0'; \
    } \
    else {\
      buf[(s)] = __lpr / 10 + '0'; \
      buf[(s)+1] = __lpr % 10 + '0'; \
    } \
  }

#define CLEAR4(s) { buf[(s)] = buf[(s)+1] = buf[(s)+2] = buf[(s)+3] = ' ';}
#define CLEAR3(s) { buf[(s)] = buf[(s)+1] = buf[(s)+2] = ' ';}
#define CLEAR2(s) { buf[(s)] = buf[(s)+1] = ' ';}

#define SELFPOS 11
#define OTHERPOS 31

GLOBAL void     statline(p, j, self)
  register aPlayer *p;			/* who we're looking at */
  register aPlayer *j;			/* who's displaying */
  int             self;

{
  static char     buf[80];
  static char     hb1[80] =
  "Flags        warp dam shd torps kills armies  fuel wtemp etemp failed    ";
  static char     hb2[80] =
  "Enemy                                                                    ";

  /* Instead of one sprintf, we do all this by hand for optimization */

  buf[0] = ISSHIELD(p) ? 'S' : ' ';
  buf[1] = colorlet[(int) p->p_alert];
  buf[2] = ISPLLOCK(p) || ISPLOCK(p) ? 'L' : ' ';
  buf[3] = ISREPAIR(p) ? 'R' : ' ';
  buf[4] = ISBOMB(p) ? 'B' : ' ';
  buf[5] = ISORBIT(p) ? 'O' : ' ';
  buf[6] = ISCLOAK(p) ? 'C' : ' ';
  buf[7] = ISWEP(p) ? 'W' : ' ';
  buf[8] = ISENG(p) ? 'E' : ' ';
  buf[9] = ISBEAMUP(p) ? 'u' : ' ';
  buf[10] = ISBEAMDOWN(p) ? 'd' : ' ';
  buf[11] = ISCOPILOT(p) ? 'P' : ' ';
  CLEAR2(12);
  {
    int             speed = p->p_speed;

    buf[14] = speed / 10 + '0';
    buf[15] = '.';
    buf[16] = speed % 10 + '0';
  }
  buf[17] = ' ';

  PRINT3(p->p_damage / DAMSCALE(1), buf, 18);
  buf[21] = ' ';
  PRINT3(p->p_shield / DAMSCALE(1), buf, 22);


  CLEAR4(25)
    if (self)
    PRINT2(p->p_ntorp, buf, 27);

  CLEAR3(29);

  {
    float           kills;

    if (!self || (kills = p->p_kills) < 0.01) {
      buf[32] = ' ';
      buf[33] = ' ';
      buf[34] = ' ';
      buf[35] = ' ';
      buf[36] = ' ';
    }
    else {
      int             temp = (int) kills;

      buf[32] = '0' + temp / 10;
      buf[33] = '0' + temp % 10;
      buf[34] = '.';
      kills = (kills - temp) * 10;
      temp = (int) kills;
      buf[35] = '0' + temp;
      kills = (kills - temp) * 10;
      temp = (int) kills;
      buf[36] = '0' + temp;
    }
  }
  CLEAR4(37);


  PRINT2(p->p_armies, buf, 41);

  CLEAR2(43);

  buf[45] = '0' + (p->p_fuel / 10000);
  if (buf[45] == '0')
    buf[45] = ' ';
  buf[46] = '0' + ((p->p_fuel % 10000) / 1000);
  if ((buf[46] == '0') && (p->p_fuel < 10000))
    buf[46] = ' ';
  buf[47] = '0' + ((p->p_fuel % 1000) / 100);
  if ((buf[47] == '0') && (p->p_fuel < 1000))
    buf[47] = ' ';
  buf[48] = '0' + ((p->p_fuel % 100) / 10);
  if ((buf[48] == '0') && (p->p_fuel < 100))
    buf[48] = ' ';
  buf[49] = '0' + (p->p_fuel % 10);

  CLEAR2(50);

  PRINT3(p->p_wtemp / TEMPSCALE(1), buf, 52);
  CLEAR3(55);

  PRINT3(p->p_etemp / TEMPSCALE(1), buf, 58);
  CLEAR3(61);

  {
    buf[64] = FAILED(p, Cloak) ? 'C' : ' ';
    buf[65] = FAILED(p, LRS) ? 'L' : ' ';
    buf[66] = FAILED(p, Phaser) ? 'P' : ' ';
    buf[67] = FAILED(p, SRS) ? 'S' : ' ';
    buf[68] = FAILED(p, Torp) ? 'T' : ' ';
    buf[69] = FAILED(p, Cooling) ? 'c' : ' ';
    buf[70] = FAILED(p, Lock) ? 'l' : ' ';
    buf[71] = FAILED(p, Shield) ? 's' : ' ';
    buf[72] = FAILED(p, Trans) ? 't' : ' ';
  }

  buf[73] = '\0';

  /* Draw status line */
  {
    int             bufpos;
    char           *hb;

    if (self) {
      hb = hb1;
      bufpos = 1;
    }
    else {
      hb = hb2;
      bufpos = 21;
    }
    if (!j->ts_offset)
      j->ts_offset = (WINSIDE - XTextWidth(j->dfont, hb, STATLINESIZE)) / 2;
    if (!(udcounter % 16))
      XDrawImageString(j->display, j->tstatw, j->dfgc,
		  j->ts_offset, bufpos + j->dfont->ascent, hb, STATLINESIZE);

    bufpos = self ? SELFPOS : OTHERPOS;

    if (!self || bcmp(j->last_msg, buf, STATLINESIZE) != 0) {
      XDrawImageString(j->display, j->tstatw, j->dfgc,
		 j->ts_offset, bufpos + j->dfont->ascent, buf, STATLINESIZE);
      if (self)
	bcopy(buf, j->last_msg, STATLINESIZE);
    }
  }
}
