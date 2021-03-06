/*
 * Copyright (c) 1992,1993,1994 Hellmuth Michaelis, Brian Dunford-Shore,
 *                              Joerg Wunsch and Scott Turner.
 *
 * Copyright (c) 1993 Charles Hannum.
 *
 * All rights reserved.
 *
 * Parts of this code regarding the NetBSD interface were written
 * by Charles Hannum.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz and Don Ahn.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by
 *	Hellmuth Michaelis, Brian Dunford-Shore, Joerg Wunsch, Scott Turner
 *	and Charles Hannum.
 * 4. The name authors may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *
 * @(#)pcvt_drv.c, 3.00, Last Edit-Date: [Wed Feb  2 19:13:14 1994]
 *
 */

/*---------------------------------------------------------------------------*
 *
 *	pcvt_drv.c	VT220 Driver Main Module / OS - Interface
 *	---------------------------------------------------------
 *	-jw	include rather primitive X stuff
 *	-hm	added netbsd 0.8 support
 *	-hm	NetBSD-current support
 *	-hm	converting to memory mapped virtual screens
 *	-hm	132 column support for vga's
 *	-hm	new pcstart debugging
 *	-hm	132 columns for WD90C11
 *	-hm	netbsd 9.0 alpha / new tty subsystem
 *	-jw	set CLOCAL in pcopen, a bug must be somewhere else ...
 *	-hm	pcprobe moved back into this file
 *	-hm	vga_string routine
 *	-hm	keybord security built into XSERVER part
 *	-jw	USL VT compatibility
 *	-hm	adding bugfixes from joerg for non USL VT X server
 *	-hm	superprobe compatibility patch from joerg
 *	-hm	support for keyboard scancode sets 1 and 2
 *	-jw	mouse emulation mode
 *	-jw/hm	all ifdef's converted to if's 
 *	-hm	bugfix from Joerg for pcrint(), wrong openf tested.
 *		removed pcxint(), because it's really not needed.
 *	-hm	applied patches from John Brezak and Szabolcs Szigeti
 *		for recent NetBSD-current differences
 *	-hm	patch from Michael Havemester for NetBSD-current 12/02/94
 *
 *---------------------------------------------------------------------------*/

#include "vt.h"
#if NVT > 0

#define EXTERN			/* allocate mem */

#include "pcvt_hdr.h"		/* global include */

unsigned	__debug = 0; /*0xffe */;
static		__color;
static		nrow;

static void vgapelinit(void);	/* read initial VGA DAC palette */

#if defined XSERVER && !PCVT_USL_VT_COMPAT
static int pcvt_xmode_set(int on, struct proc *p); /* initialize for X mode */
#endif /* XSERVER && !PCVT_USL_VT_COMPAT */

#define	DPAUSE 1


int
pcprobe(struct isa_device *dev)
{
	kbd_code_init();
	
#if PCVT_NETBSD || PCVT_FREEBSD
	return (16);
#else
	return 1;
#endif /* PCVT_NETBSD || PCVT_FREEBSD */

}

int
pcattach(struct isa_device *dev)
{
	int i;

	vt_coldmalloc();		/* allocate memory for screens */
	
#if PCVT_NETBSD || PCVT_FREEBSD

	printf("vt%d: ", dev->id_unit);

	switch(adaptor_type)
	{
		case MDA_ADAPTOR:
			printf("mda");
			break;

		case CGA_ADAPTOR:
			printf("cga");
			break;

		case EGA_ADAPTOR:
			printf("ega");
			break;

		case VGA_ADAPTOR:
			printf("%s, ", (char *)vga_string(vga_type));
			if(can_do_132col)
				printf("80/132 col");
			else
				printf("80 col");
			vgapelinit();
			break;

		default:
			printf("unknown");
			break;
	}

	if(color == 0)
		printf(", mono");
	else
		printf(", color");

	printf(", %d scr, ", totalscreens);

	switch(keyboard_type)
	{
		case KB_AT:
			printf("at-");
			break;
			
		case KB_MFII:
			printf("mf2-");
			break;
			
		default:
			printf("unknown ");
			break;
	}

	printf("kbd, [R%s]\n", PCVT_REL);

#if PCVT_NETBSD || PCVT_FREEBSD
	for(i = 0; i < totalscreens; i++)
	{
#if PCVT_NETBSD
		pc_tty[i] = ttymalloc();		
		vs[i].vs_tty = pc_tty[i];
#else
		pccons[i] = ttymalloc(pccons[i]);
		vs[i].vs_tty = pccons[i];
#endif
	}

#if PCVT_EMU_MOUSE
#if PCVT_NETBSD
	pc_tty[totalscreens] = ttymalloc(); /* the mouse emulator tty */
#else
	/* the mouse emulator tty */
	pccons[totalscreens] = ttymalloc(pccons[totalscreens]);
#endif
#endif /* PCVT_EMU_MOUSE */

#if PCVT_NETBSD
	pcconsp = pc_tty[0];
#else
	pcconsp = pccons[0];
#endif

#endif /* PCVT_NETBSD || PCVT_FREEBSD */

#else /* !PCVT_NETBSD && !PCVT_FREEBSD*/

	switch(adaptor_type)
	{
		case MDA_ADAPTOR:
			printf(" <mda");
			break;

		case CGA_ADAPTOR:
			printf(" <cga");
			break;

		case EGA_ADAPTOR:
			printf(" <ega");
			break;

		case VGA_ADAPTOR:
			printf(" <%s,", (char *)vga_string(vga_type));
			if(can_do_132col)
				printf("80/132 col");
			else
				printf("80 col");
			vgapelinit();
			break;

		default:
			printf(" <unknown");
			break;
	}

	if(color == 0)
		printf(",mono");
	else
		printf(",color");

	printf(",%d scr,", totalscreens);
	
	switch(keyboard_type)
	{
		case KB_AT:
			printf("at-");
			break;
			
		case KB_MFII:
			printf("mf2-");
			break;
			
		default:
			printf("unknown ");
			break;
	}

	printf("kbd,[R%s]>", PCVT_REL);

#endif  /* PCVT_NETBSD || PCVT_FREEBSD */

	async_update(0);		/* start asynchronous updates */
	return 1;
}

/* had a look at the friedl driver */

#if !PCVT_NETBSD

struct tty *
get_pccons(Dev_t dev)
{
	register int i = minor(dev);
#if PCVT_EMU_MOUSE
 	if(i == totalscreens)
 		return(pccons[i]);
#endif /* PCVT_EMU_MOUSE */

	if(i >= PCVT_NSCREENS)
		return(NULL);
	return(pccons[i]);
}

#else

struct tty *
get_pccons(Dev_t dev)
{
	register int i = minor(dev);

#if PCVT_EMU_MOUSE
	if(i == totalscreens)
		return(pc_tty[i]);
#endif /* PCVT_EMU_MOUSE */

	if(i >= PCVT_NSCREENS)
		return(NULL);
	return(pc_tty[i]);
}

#endif /* !PCVT_NETBSD */

/*---------------------------------------------------------------------------*
 *		/dev/ttyc0, /dev/ttyc1, etc.
 *---------------------------------------------------------------------------*/
int
pcopen(Dev_t dev, int flag, int mode, struct proc *p)
{
	register struct tty *tp;
	register struct video_state *vsx;
	int i = minor(dev);
	
#if PCVT_EMU_MOUSE
	if(i == totalscreens)
		vsx = 0;
	else
#endif /* PCVT_EMU_MOUSE */

	vsx = &vs[i];

  	if((tp = get_pccons(dev)) == NULL)
		return ENXIO;

#if PCVT_EMU_MOUSE
	if(i == totalscreens)
	{
		if(mouse.opened == 0)
			mouse.buttons = mouse.extendedseen =
				mouse.breakseen = mouse.lastmove.tv_sec = 0;
		mouse.minor = i;
		mouse.opened++;
	}
	else
#endif /* PCVT_EMU_MOUSE */

	vsx->openf++;
	
	tp->t_oproc = pcstart;
	tp->t_param = pcparam;
	tp->t_dev = dev;

	if ((tp->t_state & TS_ISOPEN) == 0)
	{
		tp->t_state |= TS_WOPEN;
		ttychars(tp);
		tp->t_iflag = TTYDEF_IFLAG;
		tp->t_oflag = TTYDEF_OFLAG;
		tp->t_cflag = TTYDEF_CFLAG;
		tp->t_lflag = TTYDEF_LFLAG;
		tp->t_ispeed = tp->t_ospeed = TTYDEF_SPEED;
		pcparam(tp, &tp->t_termios);
		ttsetwater(tp);
	}
	else if (tp->t_state & TS_XCLUDE && p->p_ucred->cr_uid != 0)
		return (EBUSY);

	tp->t_state |= TS_CARR_ON;
	tp->t_cflag |= CLOCAL;	/* cannot be a modem (:-) */

#if PCVT_NETBSD
	return ((*linesw[tp->t_line].l_open)(dev, tp));
#else
	return ((*linesw[tp->t_line].l_open)(dev, tp, flag));
#endif
}

int
pcclose(Dev_t dev, int flag, int mode, struct proc *p)
{
	register struct tty *tp;
	register struct video_state *vsx;
	int i = minor(dev);
	
#if PCVT_EMU_MOUSE
	if(i == totalscreens)
		vsx = 0;
	else
#endif /* PCVT_EMU_MOUSE */

	vsx = &vs[i];
	
	if((tp = get_pccons(dev)) == NULL)
		return ENXIO;

	(*linesw[tp->t_line].l_close)(tp, flag);
	ttyclose(tp);

#if PCVT_EMU_MOUSE
	if(i == totalscreens)
		mouse.opened = 0;
	else
#endif /* PCVT_EMU_MOUSE */

	vsx->openf = 0;
	
#if PCVT_USL_VT_COMPAT
#if PCVT_EMU_MOUSE

	if(i == totalscreens)
		return (0);

#endif /* PCVT_EMU_MOUSE */

	if(vsx->vt_status & VT_WAIT_ACT)
		wakeup((caddr_t)&vsx->smode);
	vsx->proc = 0;
	vsx->vt_status = vsx->pid = 0;
	vsx->smode.mode = VT_AUTO;

#endif /* PCVT_USL_VT_COMPAT */
	
	return(0);
}

int
pcread(Dev_t dev, struct uio *uio, int flag)
{
	register struct tty *tp;

	if((tp = get_pccons(dev)) == NULL)
		return ENXIO;
		
#if PCVT_FORCE8BIT
	/* this does not belong to here, but anybody always wants to
	   strip the 8th bit, very likely the shell */

	tp->t_iflag &= ~ISTRIP;		
#endif /* PCVT_FORCE8BIT */

	return ((*linesw[tp->t_line].l_read)(tp, uio, flag));
}

int
pcwrite(Dev_t dev, struct uio *uio, int flag)
{
	register struct tty *tp;

	if((tp = get_pccons(dev)) == NULL)
		return ENXIO;

	return ((*linesw[tp->t_line].l_write)(tp, uio, flag));
}

int
pcioctl(Dev_t dev, int cmd, caddr_t data, int flag, struct proc *p)
{
	register error;
	register struct tty *tp;

	if((tp = get_pccons(dev)) == NULL)
		return(ENXIO);

	/* note that some ioctl's are global, e.g.  KBSTPMAT: There is
	 * only one keyboard and different repeat rates for instance between
	 * sessions are a suspicious wish. If you really need this make the
	 * appropriate variables arrays
	 */

#if PCVT_EMU_MOUSE
	if(minor(dev) == totalscreens)
	{
		if((error = mouse_ioctl(dev, cmd, data)) >= 0)
			return error;
		goto do_standard;
	}
#endif /* PCVT_EMU_MOUSE */

#ifdef XSERVER
#if PCVT_USL_VT_COMPAT

	if((error = usl_vt_ioctl(dev, cmd, data, flag, p)) >= 0)
		return error;

	/*
	 * just for compatibility:
	 * XFree86 < 2.0 and SuperProbe still might use it
	 *
	 * NB: THIS IS A HACK! Do not use it unless you explicitly need.
	 * Especially, since the vty is not put into process-controlled
	 * mode (this would require the application to co-operate), any
	 * attempts to switch vtys while this kind of X mode is active
	 * may cause serious trouble.
	 */
	switch(cmd)
	{
	case CONSOLE_X_MODE_ON:
	  {
	    int i;

	    if((error = usl_vt_ioctl(dev, KDENABIO, 0, flag, p)) > 0)
	      return error;
	    i = KD_GRAPHICS;
	    if((error = usl_vt_ioctl(dev, KDSETMODE, (caddr_t)&i, flag, p))
	       > 0)
	      return error;
	    i = K_RAW;
	    error = usl_vt_ioctl(dev, KDSKBMODE, (caddr_t)&i, flag, p);
	    return error;
	  }

	case CONSOLE_X_MODE_OFF:
	  {
	    int i;

	    (void)usl_vt_ioctl(dev, KDDISABIO, 0, flag, p);
	  
	    i = KD_TEXT;
	    (void)usl_vt_ioctl(dev, KDSETMODE, (caddr_t)&i, flag, p);
	    
	    i = K_XLATE;
	    (void)usl_vt_ioctl(dev, KDSKBMODE, (caddr_t)&i, flag, p);
	    return 0;
	  }


	case CONSOLE_X_BELL:
		/*
		 * If `data' is non-null, it points to int[2], the first
		 * value denotes the pitch in Hz, the second a duration
		 * in ms (??? jw - 333 us). Otherwise, behaves like BEL.
		 */
		if (data)
			sysbeep(PCVT_SYSBEEPF / ((int *)data)[0],
				((int *)data)[1] * hz / 3000);
		else
			sysbeep(PCVT_SYSBEEPF / 1493, hz / 4);
		return (0);

	default: /* fall through */ ;
	}

#else /* PCVT_USL_VT_COMPAT */

	switch(cmd)
	{
	case CONSOLE_X_MODE_ON:
		return pcvt_xmode_set(1, p);

	case CONSOLE_X_MODE_OFF:
		return pcvt_xmode_set(0, p);

	case CONSOLE_X_BELL:
		/*
		 * If `data' is non-null, it points to int[2], the first
		 * value denotes the pitch in Hz, the second a duration
		 * in ms (??? jw - 333 us). Otherwise, behaves like BEL.
		 */
		if (data)
			sysbeep(PCVT_SYSBEEPF / ((int *)data)[0],
				((int *)data)[1] * hz / 3000);
		else
			sysbeep(PCVT_SYSBEEPF / 1493, hz / 4);
		return (0);

	default: /* fall through */ ;
	}

#endif /* PCVT_USL_VT_COMPAT */
#endif /* XSERVER */

	if((error = kbdioctl(dev,cmd,data,flag)) >= 0)
		return error;

	if((error = vgaioctl(dev,cmd,data,flag)) >= 0)
		return error;

do_standard:
#ifndef NETBSD_CURRENT_12_02_94
	if((error = (*linesw[tp->t_line].l_ioctl)(tp, cmd, data, flag)) >= 0)
#else
	if((error = (*linesw[tp->t_line].l_ioctl)(tp, cmd, data, flag, p)) >= 0)
#endif
		return (error);

#ifndef NETBSD_CURRENT_12_02_94
	if((error = ttioctl(tp, cmd, data, flag)) >= 0)
#else
	if((error = ttioctl(tp, cmd, data, flag, p)) >= 0)
#endif
		return (error);

	return (ENOTTY);
}

int
pcmmap(Dev_t dev, int offset, int nprot)
{
	if (offset > 0x20000)
		return -1;
	return i386_btop((0xa0000 + offset));
}

/*---------------------------------------------------------------------------*
 *
 *	handle a keyboard receive interrupt
 *
 *	NOTE: the keyboard is multiplexed by means of "pcconsp"
 *	between virtual screens. pcconsp - switching is done in
 *	the vgapage() routine
 *
 *---------------------------------------------------------------------------*/
void
pcrint(Dev_t dev, int irq, int cpl)
{
	u_char *cp;
	
#if PCVT_SCREENSAVER
	pcvt_scrnsv_reset();
#endif /* PCVT_SCREENSAVER */

	if((cp = sgetc(1)) == 0)
		return;

	if (kbd_polling)
		return;

	if(!(vs[current_video_screen].openf))	/* XXX was vs[minor(dev)] */
		return;
	
#if PCVT_NULLCHARS
	if(*cp == 0)
	{
		/* pass a NULL character */
		(*linesw[pcconsp->t_line].l_rint)(0, pcconsp);
		return;
	}
#endif /* PCVT_NULLCHARS */

	while (*cp)
		(*linesw[pcconsp->t_line].l_rint)(*cp++ & 0xff, pcconsp);
}


#if PCVT_NETBSD

void
pcstart(register struct tty *tp)
{
	register struct clist *rbp;
	int s, len, n;
	u_char buf[PCVT_PCBURST];

	s = spltty();

	if (tp->t_state & (TS_TIMEOUT|TS_BUSY|TS_TTSTOP))
		goto out;

	tp->t_state |= TS_BUSY;

	splx(s);

	/*
	 * We need to do this outside spl since it could be fairly
	 * expensive and we don't want our serial ports to overflow.
	 */

	rbp = &tp->t_outq;

	len = q_to_b(rbp, buf, PCVT_PCBURST);

	for (n = 0; n < len; n++)
	{
		if (buf[n])
			sput(buf[n], 0, minor(tp->t_dev));
	}

	s = spltty();

	tp->t_state &= ~TS_BUSY;

	if (rbp->c_cc)
	{
		tp->t_state |= TS_TIMEOUT;
		timeout((timeout_t)ttrstrt, (caddr_t)tp, 1);
	}

	if (rbp->c_cc <= tp->t_lowat)
	{
		if (tp->t_state&TS_ASLEEP)
		{
			tp->t_state &= ~TS_ASLEEP;
			wakeup((caddr_t)rbp);
		}
		selwakeup(&tp->t_wsel);
	}
out:
	splx(s);
}

#else /* !PCVT_NETBSD */	/* 386BSD 0.1 || FreeBSD */

void
pcstart(struct tty *tp)
{
	int c, s;
	
	s = spltty();

	if (tp->t_state & (TS_TIMEOUT|TS_BUSY|TS_TTSTOP))
	{
		goto out;
	}
	
	for(;;)
	{
		if (RB_LEN(tp->t_out) <= tp->t_lowat)
		{
			if (tp->t_state&TS_ASLEEP)
			{
				tp->t_state &= ~TS_ASLEEP;
				wakeup((caddr_t)tp->t_out);
			}

			if (tp->t_wsel)
			{
				selwakeup(tp->t_wsel, tp->t_state & TS_WCOLL);
				tp->t_wsel = 0;
				tp->t_state &= ~TS_WCOLL;
			}
		}

		if (RB_LEN(tp->t_out) == 0)
		{
			goto out;
		}

		c = getc(tp->t_out);

		tp->t_state |= TS_BUSY;	/* patch from Frank Maclachlan */
		splx(s);
		sput(c, 0, minor(tp->t_dev));
		spltty();
		tp->t_state &= ~TS_BUSY; /* patch from Frank Maclachlan */
	}
out:
	splx(s);
}

#endif /* PCVT_NETBSD */

/*---------------------------------------------------------------------------*
 *		/dev/console
 *---------------------------------------------------------------------------*/

#if !PCVT_NETBSD	/* has moved to cons.c in netbsd-current */
void
consinit()		/* init for kernel messages during boot */
{
}
#endif /* PCVT_NETBSD */

int
pccnprobe(struct consdev *cp)
{
	int maj;

	/* locate the major number */
	
	for (maj = 0; maj < nchrdev; maj++)
	{
		if ((u_int)cdevsw[maj].d_open == (u_int)pcopen)
			break;
	}
	
	/* initialize required fields */

	cp->cn_dev = makedev(maj, 0);
	cp->cn_pri = CN_INTERNAL;

#if !PCVT_NETBSD
	cp->cn_tp = pccons[0];
#endif /* !PCVT_NETBSD */
	return 1;
}

int
pccninit(struct consdev *cp)
{
	return 0;
}

int
pccnputc(Dev_t dev, U_char c)
{
	if (c == '\n')
		sput('\r', 1, 0);
	sput(c, 1, 0);
	return 0;
}


int
pccngetc(Dev_t dev)
{
	register int s;
	register u_char *cp;

#ifdef XSERVER
#if !PCVT_USL_VT_COMPAT
	if (pcvt_xmode)
		return 0;
#else /* !PCVT_USL_VT_COMPAT */
	if (pcvt_kbd_raw)
		return 0;
#endif /* !PCVT_USL_VT_COMPAT */
#endif /* XSERVER */

	s = spltty();		/* block pcrint while we poll */
	cp = sgetc(0);
	splx(s);

	if (*cp == '\r')
		return('\n');
	return (*cp);
}

/*---------------------------------------------------------------------------*
 *	Set line parameters
 *---------------------------------------------------------------------------*/
int
pcparam(struct tty *tp, struct termios *t)
{
	register int cflag = t->c_cflag;

        /* and copy to tty */

        tp->t_ispeed = t->c_ispeed;
        tp->t_ospeed = t->c_ospeed;
        tp->t_cflag = cflag;

	return(0);
}

#if PCVT_NEEDPG

/* this is moved to cons.c in patchkit 0.2.2 and higher */

int
pg (char *p,
    int q, int r, int s, int t, int u,
    int v, int w, int x, int y, int z)
{
#if !PCVT_USL_VT_COMPAT
	vgapage(0);
#else
	switch_screen(0, 0);
#endif /* !PCVT_USL_VT_COMPAT */
	printf(p,q,r,s,t,u,v,w,x,y,z);
	printf("\n");
	return(getchar());
}

#endif /* PCVT_NEEDPG */

/* special characters */
#define bs	8
#define lf	10	
#define cr	13	
#define cntlc	3	
#define del	0177	
#define cntld	4

int
getchar(void)
{
	u_char	thechar;
	int	x;

	kbd_polling = 1;

	x = splhigh();

	sput('>', 1, 0);
	thechar = *(sgetc(0));

	kbd_polling = 0;

	splx(x);

	switch (thechar)
	{
		default:
			if (thechar >= ' ')
				sput(thechar, 1, 0);
			return(thechar);

		case cr:
		case lf:
			sput('\r', 1, 0);
			sput('\n', 1, 0);
			return(lf);

		case bs:
		case del:
			 sput('\b', 1, 0);
			 sput(' ', 1, 0);
			 sput('\b', 1, 0);
			 return(thechar);

		case cntlc:
			 sput('^', 1, 0) ; sput('C', 1, 0) ; sput('\r', 1, 0) ; sput('\n', 1, 0) ;
			 cpu_reset();

		case cntld:
			 sput('^', 1, 0) ; sput('D', 1, 0) ; sput('\r', 1, 0) ; sput('\n', 1, 0) ;
			 return(0);
	}
}

void
dprintf(unsigned flgs, const char *fmt, ...)
{
	extern unsigned __debug;
	va_list ap;

	if((flgs&__debug) > DPAUSE)
	{
		__color = ffs(flgs&__debug)+1;
		va_start(ap,fmt);
		kprintf(fmt, 1, (struct tty *)0, ap);
		va_end(ap);

		if (flgs & DPAUSE || nrow%24 == 23)
		{ 
			int x;
			x = splhigh();
			if(nrow%24 == 23)
				nrow = 0;
			(void)sgetc(0);
			splx(x);
		}
	}
	__color = 0;
}

/*----------------------------------------------------------------------*
 *	read initial VGA palette (as stored by VGA ROM BIOS) into
 *	palette save area
 *----------------------------------------------------------------------*/
void
vgapelinit(void)
{
	register unsigned idx;
	register struct rgb *val;

	/* first, read all and store to first screen's save buffer */
	for(idx = 0, val = vs[0].palette; idx < NVGAPEL; idx++, val++)
		vgapaletteio(idx, val, 0 /* read it */);

	/* now, duplicate for remaining screens */
	for(idx = 1; idx < PCVT_NSCREENS; idx++)
		bcopy(vs[0].palette, vs[idx].palette,
		      NVGAPEL * sizeof(struct rgb));
}

#if defined XSERVER && !PCVT_USL_VT_COMPAT
/*----------------------------------------------------------------------*
 *	initialize for X mode
 *	i.e.: grant current process (the X server) all IO priviledges,
 *	and mark in static variable so other hooks can test for it,
 *	save all loaded fonts and screen pages to pageable buffers;
 *	if parameter `on' is false, the same procedure is done reverse.
 *----------------------------------------------------------------------*/
static int
pcvt_xmode_set(int on, struct proc *p)
{
	static unsigned char *saved_fonts[NVGAFONTS];
	
#if PCVT_SCREENSAVER
	static unsigned saved_scrnsv_tmo = 0;
#endif /* PCVT_SCREENSAVER */

#if PCVT_NETBSD
	extern u_short *Crtat;
#endif /* PCVT_NETBSD */

#if (PCVT_NETBSD > 9) || (PCVT_FREEBSD && PCVT_FREEBSD > 102)
	struct trapframe *fp;
#else
	struct syscframe *fp;
#endif	
	int i;

	if(adaptor_type != VGA_ADAPTOR
	   && adaptor_type != MDA_ADAPTOR)
		/* X will only run on those adaptors */
		return (EINVAL);

#if PCVT_NETBSD > 9
	fp = (struct trapframe *)p->p_regs;
#else
	fp = (struct syscframe *)p->p_regs;
#endif	

	if(on)
	{
		/*
		 * Test whether the calling process has super-user priviledges.
		 * This prevents us from granting the potential security hole
		 * `IO priv' to any process (effective uid is checked).
		 */
		if(suser(p->p_ucred, &p->p_acflag) != 0)
			return (EPERM);

		if(pcvt_xmode)
			return 0;
		pcvt_xmode = pcvt_kbd_raw = 1;

		for(i = 0; i < totalfonts; i++) {
			if(vgacs[i].loaded) {
				saved_fonts[i] = (unsigned char *)
					malloc(32 * 256, M_DEVBUF, M_WAITOK);
				if(saved_fonts[i] == 0) {
					printf(
				"pcvt_xmode_set: no font buffer available\n");
					return (EAGAIN);
				}
				else
					vga_move_charset(i, saved_fonts[i], 1);
			} else
				saved_fonts[i] = 0;
		}

#if PCVT_SCREENSAVER
		if(saved_scrnsv_tmo = scrnsv_timeout)
			pcvt_set_scrnsv_tmo(0);	/* turn it off */
#endif /* PCVT_SCREENSAVER */

		async_update(1);	/* turn off */

		/* disable text output and save screen contents */
		/* video board memory -> kernel memory */
		bcopyb(Crtat, vsp->Memory,
		       vsp->screen_rowsize * vsp->maxcol * CHR);

		vsp->Crtat = vsp->Memory;	/* operate in memory now */

#if PCVT_SCANSET == 2
		/* put keyboard to return ancient PC scan codes */
		kbc_8042cmd(CONTR_WRITE); 
#if PCVT_USEKBDSEC		/* security enabled */
		outb(CONTROLLER_DATA,
		 (COMMAND_SYSFLG|COMMAND_IRQEN|COMMAND_PCSCAN));
#else				/* security disabled */
		outb(CONTROLLER_DATA,
		 (COMMAND_INHOVR|COMMAND_SYSFLG|COMMAND_IRQEN|COMMAND_PCSCAN));
#endif /* PCVT_USEKBDSEC */
#endif /* PCVT_SCANSET == 2 */

#if PCVT_NETBSD > 9
		fp->tf_eflags |= PSL_IOPL;
#else
		fp->sf_eflags |= PSL_IOPL;
#endif		
	}
	else
	{
		if(!pcvt_xmode)
			return 0;
		pcvt_xmode = pcvt_kbd_raw = 0;

		for(i = 0; i < totalfonts; i++)
			if(saved_fonts[i]) {
				vga_move_charset(i, saved_fonts[i], 0);
				free(saved_fonts[i], M_DEVBUF);
				saved_fonts[i] = 0;
			}

#if PCVT_NETBSD > 9
		fp->tf_eflags &= ~PSL_IOPL;
#else
		fp->sf_eflags &= ~PSL_IOPL;
#endif		

#if PCVT_SCREENSAVER
		if(saved_scrnsv_tmo)
			pcvt_set_scrnsv_tmo(saved_scrnsv_tmo);
#endif /* PCVT_SCREENSAVER */

#if PCVT_SCANSET == 2
		kbc_8042cmd(CONTR_WRITE); 
#if PCVT_USEKBDSEC		/* security enabled */
		outb(CONTROLLER_DATA,
		 (COMMAND_SYSFLG|COMMAND_IRQEN));
#else				/* security disabled */
		outb(CONTROLLER_DATA,
		 (COMMAND_INHOVR|COMMAND_SYSFLG|COMMAND_IRQEN));
#endif /* PCVT_USEKBDSEC */
#endif /* PCVT_SCANSET == 2 */

		if(adaptor_type == MDA_ADAPTOR)
		  {
		    /*
		     * Due to the fact that HGC registers are write-only,
		     * the Xserver can only make guesses about the state
		     * the HGC adaptor has been before turning on X mode.
		     * Thus, the display must be re-enabled now, and the
		     * cursor shape and location restored.
		     */
		    outb(GN_DMCNTLM, 0x28); /* enable display, text mode */
		    outb(addr_6845, CRTC_CURSORH); /* select high register */
		    outb(addr_6845+1,
			 ((vsp->Crtat + vsp->cur_offset) - Crtat) >> 8);
		    outb(addr_6845, CRTC_CURSORL); /* select low register */
		    outb(addr_6845+1,
			 ((vsp->Crtat + vsp->cur_offset) - Crtat));

		    outb(addr_6845, CRTC_CURSTART); /* select high register */
		    outb(addr_6845+1, vsp->cursor_start);
		    outb(addr_6845, CRTC_CUREND); /* select low register */
		    outb(addr_6845+1, vsp->cursor_end);
		  }

		/* restore screen and re-enable text output */
		/* kernel memory -> video board memory */
		bcopyb(vsp->Memory, Crtat,
		       vsp->screen_rowsize * vsp->maxcol * CHR);

		vsp->Crtat = Crtat;	/* operate on-screen now */
	
		async_update(0);
	}
	return 0;
}
#endif	/* XSERVER && !PCVT_USL_VT_COMPAT */

#if PCVT_386BSD
/* dummies required to work with patchkit 0.2.4 */

void cons_highlight (void) {}
void cons_normal (void) {}
#endif

#endif	/* NVT > 0 */

/*-------------------------- E O F -------------------------------------*/
