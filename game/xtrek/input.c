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
#include <X11/Xos.h>
#include <X11/Xutil.h>

#include <stdio.h>
#include <math.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <errno.h>
#include <signal.h>
#ifdef CHAINSAW
#include <setjmp.h>
jmp_buf ioerrorbuf;
struct player *ioerrorplayer;
#endif /* CHAINSAW */

#if defined(ibm032) || defined(sony)
extern int errno;
#endif

#include "defs.h"
#include "data.h"
#include "polar.h"
#include "keys.h"

#ifndef FD_SET
#define	FD_SET(n, s)	(((s)->fds_bits[0]) |= (1 << n))
#define	FD_CLR(n, s)	(((s)->fds_bits[0]) &= ~(1 << n))
#define	FD_ZERO(s)	bzero((char *)(s), sizeof (*(s)))
#define	FD_ISSET(n, s)	(((s)->fds_bits[0]) & (1 << n))
#endif

static int      do_the_redraw, skipUpdates = 1;
int             peerdied;

#define INITMASKS (ButtonPressMask|ButtonReleaseMask|ExposureMask)
#define INITKEYMASKS (KeyPressMask | INITMASKS)

#define REMOVE_PLAYER(p) { \
  	    playerchange = 1; \
  	    XCloseDisplay (p->display); \
  	    p->display = 0; \
  	    p->p_status = PFree; \
  	    nplayers--; \
            CLRENTER(p); }

void      setRedrawFlag()
{
  if (skipUpdates)
    do_the_redraw = 1;
  else
    do_the_redraw++;

  /* get around a SYSV signal misfeature */
#if defined(USG) || defined(SYSV)
  signal(SIGALRM, setRedrawFlag);
#endif
}

LOCAL void      deadpeer()
{
  fprintf(stderr, "SIGPIPE\n");
  peerdied = 1;
}

LOCAL int       inputIgnored(p)
  register struct player *p;
{

  if (p->p_status != PAlive)
    return 1;
  if (!ISWAR(p))
    return 0;
  warning(p, "Battle computers being re-programmed.");
  return 1;
}

#ifdef USEWATCH
LOCAL void      setwatch(pno)
  int             pno;
{
  register struct player *p = &players[pno];

  p->lastm = mctl->mc_current;
  p->redrawall = 1;
  if (!ISSHOWSTATS(p))
    return;
  closeStats(p, p->statwin);
  p->statwin = openStats(p);
}

#endif

LOCAL u_char       getcourse(p, ww, x, y)
  register struct player *p;
  Window          ww;
  int             x, y;
{
  return ww != p->mapw ?
    POLAR_DIRECTION(x - WINSIDE / 2, y - WINSIDE / 2) :
    POLAR_DIRECTION(x * GWIDTH / WINSIDE - p->p_x, y * GWIDTH / WINSIDE - p->p_y);
}

LOCAL void      buttonaction(p, data)
  register struct player *p;
  XButtonEvent   *data;
{
  u_char           course;

  if ((data->button & Button3) == Button3) {
    course = getcourse(p, data->window, data->x, data->y);
    p->p_desdir = course;
    CLRPLLOCK(p);
    CLRFOLLOW(p);
  }
  else if ((data->button & Button1) == Button1) {
    course = getcourse(p, data->window, data->x, data->y);
    ntorp(p, course, TMove, -1);
  }
  else if ((data->button & Button2) == Button2) {
    course = getcourse(p, data->window, data->x, data->y);
    phaser(p, course);
  }
}

LOCAL void      keyaction(p, key, data)
  struct player  *p;
  char            key;
  XKeyEvent      *data;
{
  char            buf[80];
  u_char           course;
  struct obtype  *gettarget(), *target;
  struct player  *p2;
  struct planet  *pl;

  switch (key) {
      case '\004':
	  {
	  extern int debug;
	  debug=1;
	  break;
	  }
    case FRACWARP_KEY:
      p->fracwarp = 1;
      break;
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':{
	short           speed = key - '0';

	if (p->fracwarp) {
	  p->fracwarp = 0;
	  set_speed(p, ((int) (p->p_desspeed / 10) * 10) + speed);
	}
	else
	  set_speed(p, speed * 10);
      }
      break;
    case SETCOURSE_KEY:		/* set course */
      course = getcourse(p, data->window, data->x, data->y);
      set_course(p, course);
      CLRPLLOCK(p);
      CLRFOLLOW(p);
      break;
    case PHASER_KEY:			/* fire phasers */
      course = getcourse(p, data->window, data->x, data->y);
      phaser(p, course);
      break;
    case MINE_KEY:			/* drop mine */
      if (g_mine)
        ntorp(p, 0, TMine, -1);
      else
        warning(p,"Mines disabled");
      break;
    case TOGGLE_BELL_KEY:
      p->do_bell = !p->do_bell;
      break;
    case TORP_KEY:			/* t = launch torps */
      course = getcourse(p, data->window, data->x, data->y);
      ntorp(p, course, TMove, -1);
      break;
    case DET_OTHER_TORP_KEY:		/* detonate other torps */
      detothers(p);
      break;
    case DET_OWN_TORP_KEY:		/* explode own torps */
      detTorp(p);
      break;
    case DET_OWN_MINE_KEY:		/* Detonate own mines */
      detMine(p);
      break;

#ifdef TURBO_OPTION
    case TURBO_KEY:			/* Turbo */
      if (g_turbo)
        turbo(p);
      else
        warning(p,"Turbo disabled");
      break;
#endif

#ifdef TELEPORT_OPTION
    case TELEPORT_KEY:			/* jump (teleport) */
      if (g_teleport)
        teleport(p);
      else
        warning(p,"Teleport disabled");
      break;
#endif

    case SHIELDS_UP_KEY:		/* Put shields up */
      shield_up(p);
      break;

    case SHIELDS_DOWN_KEY:		/* Put shields down */
      shield_down(p);
      break;
    case TOGGLE_SHIELDS_KEY:		/* toggle shields */
      shield_tog(p);
      break;
    case BOMB_KEY:			/* bomb planet */
      bomb_planet(p);
      break;
    case BEAM_UP_KEY:			/* z = beam up */
      beam_up(p);
      break;
    case BEAM_DOWN_KEY:		/* x = beam down */
      beam_down(p);
      break;
    case FOLLOW_KEY:			/* follow a ship */
      follow_ship(p);
      break;
    case REPAIR_KEY:			/* R = Go into repair mode */
      repair(p);
      break;
    case ORBIT_KEY:			/* o = orbit nearest planet */
      orbit(p);
      break;
    case ROBOT_HORDE_KEY:		/* bring in robots (this is a HOSE) */
      inrobots ^= 1;
      break;
    case QUIT_KEY:
      if (!g_selfdestruct) {
        warning(p,"Self Destruct disabled");
        break;
      }
      if (p->copilot /* || watch */ )
	exit(1);
      SETSELFDEST(p); {
	int             slowdown = g_fastdestruct ? 1 : 1 << ((int) p->p_alert);

	p->selfdest = p->p_updates + PSELFDESTTIME * slowdown;
	warning(p, "Self destruct initiated.");
      }
      break;
    case REPEAT_MESSAGE_KEY:		/* Redisplay all messages */
      repeat_message(p);
      break;
    case CLOAK_KEY:			/* c = cloak */
      if (!g_cloak) {
        warning(p,"Cloaking disabled");
        break;
      }
      if (ISCLOAK(p))
	cloak_off(p);
      else
	cloak_on(p);
      break;
    case COUP_KEY:			/* C = coups */
      coup(p);
      break;
    case LOCK_KEY:			/* l = lock onto */
      if (FAILED(p, Lock))
	break;
      /* since a robot would never use this function (it's user Interface
         dependent,) all the work is done here instead of in interface.c */
      target = gettarget(p, data->window, data->x, data->y,
			 TARG_PLAYER | TARG_PLANET);
      if (target->o_type == PLAYERTYPE) {
	SETPLOCK(p);
	CLRFOLLOW(p);
	p->p_playerl = target->o_num;
	p2 = &players[target->o_num];
	sprintf(buf, "Locked on to %s (%c%d).",
		p2->p_name, empires[p2->p_empire].code, p2->p_no);
	warning(p, buf);
      }
      else {				/* It's a planet */
	SETPLLOCK(p);
	CLRORBIT(p);
	CLRBEAMUP(p);
	CLRBEAMDOWN(p);
	CLRBOMB(p);
	CLRFOLLOW(p);
	p->p_planet = target->o_num;
	pl = &planets[target->o_num];
	sprintf(buf, "Locked on to %s.", pl->pl_name);
	warning(p, buf);
      }
      break;
    case TOGGLE_COPILOT_KEY:		/* toggle copilot permissions */
      FLIPCOPILOT(p);
      break;
    case SETRHOSTILE_KEY:
      SETRHOSTILE(p);
      break;
    case CLRRHOSTILE_KEY:
      CLRRHOSTILE(p);
      break;
    case EASY_ROBOT_KEY:		/* * = send in practice robot */
      /* Only if no other players on OTHER empires. */
      if (tcount[p->p_empire] - (nplayers - nrobots) == 0) {
	startrobot(p->p_empire, PFRHOSTILE, 0, 0);
	SETRHOSTILE(p);
      }
      break;
    case HARD_ROBOT_KEY:		/* & = send in harder robot */
      /* Only if no other players on OTHER empires. */
      if (tcount[p->p_empire] - (nplayers - nrobots) == 0) {
	startrobot(p->p_empire, PFRHARD | PFRHOSTILE, 0, 0);
	SETRHOSTILE(p);
      }
      break;
      /* Start of display functions */
    case CLEAR_WINDOW_KEY:		/* clear special windows */
      if (ismapped(p, p->playerw))
	XUnmapWindow(p->display, p->playerw);
      if (ismapped(p, p->planetw))
	XUnmapWindow(p->display, p->planetw);
      if (p->infomapped)
	destroyInfo(p);
      if (ismapped(p, p->war))
	XUnmapWindow(p->display, p->war);
      break;
    case PLAYER_LIST_KEY:		/* L = Player list */
      ToggleWindow(p, playerw);
      break;
    case PLANET_LIST_KEY:		/* P = Planet list */
      ToggleWindow(p, planetw);
      break;
    case SCORE_LIST_KEY:		/* Score list */
      if (p->infomapped)
	destroyInfo(p);
      else
	scorelist(p);
      break;
    case TOGGLE_STAT_KEY:		/* s = toggle stat mode */
      if (p->statwin) {
	closeStats(p, p->statwin);
	CLRSHOWSTATS(p);
      }
      else {
	p->statwin = openStats(p);
	SETSHOWSTATS(p);
      }
      break;
    case TOGGLE_SHOW_SHIELDS_KEY:	/* toggle show shields */
      p->showShields = !p->showShields;
      break;
    case TOGGLE_MAP_MODE_KEY:		/* M = Toggle Map mode */
      p->mapmode = !p->mapmode;
      break;
    case TOGGLE_NAME_MODE_KEY:		/* N = Toggle Name mode */
      p->namemode = !p->namemode;
      break;
    case INFO_KEY:			/* i = get information */
      if (p->infomapped)
	destroyInfo(p);
      else
	inform(p, data->window, data->x, data->y);
      break;
    case HELP_KEY:			/* Map help window */
      ToggleWindow(p, helpWin);
      break;
    case WAR_KEY:			/* w = map war stuff */
      if (p->copilot) {
	warning(p, "Copilots cannot alter war settings.");
	break;
      }
      if (ismapped(p, p->war)) {
	XUnmapWindow(p->display, p->war);
	p->redrawall = 1;
      }
      else
	warwindow(p);
      break;
    default:
      if (p->do_bell)
	XBell(p->display, 0);
      sprintf(buf,"Unknown command: '%c'", key);
      warning(p, buf);
      break;
  }
}

LOCAL void      SetTimer()
{
  struct itimerval udt;

  udt.it_interval.tv_sec = 0L;
  udt.it_interval.tv_usec = UPDATE;
  udt.it_value.tv_sec = 0L;
  udt.it_value.tv_usec = UPDATE;
  setitimer(ITIMER_REAL, &udt, 0);
}

LOCAL int       CountPlayers(old_num_players)
  int             old_num_players;
{
  int             np = 0;
  struct player  *p;

  for (p = &players[0]; p < &players[MAXPLAYER]; p++)
    if (p->p_status != PFree)
      np++;

  return np;
}

LOCAL int       CheckSelectWindow(player, win)
  aPlayer        *player;
  Window          win;
{
  int             empire;

  for (empire = 0; empire < numempires; empire++)
    if (win == player->win[empire]) {
      del_entrywindow(player);
      XClearWindow(player->display, player->w);
      CLRENTER(player);
      enter(empire, XDisplayString(player->display), player - players);
      return 1;
    }
  if (win == player->qwin)
    return -1;
  return 0;
}

#ifdef CHAINSAW
LOCAL int xtrekioerrorhandler(dpy)
Display *dpy;
{
    extern int sys_nerr;
    extern char *sys_errlist[];
    char *msg;

    msg = ((errno >= 0 && errno < sys_nerr) ?
	   sys_errlist[errno] : "unknown error");
    fprintf (stderr, 
	     "XIO:  fatal IO error %d (%s) on X server \"%s\"\r\n",
	     errno, msg, DisplayString (dpy));
    if (dpy != ioerrorplayer->display) {
	fprintf(stderr, "XIO error on wrong player!\n");
	exit(1);
    }
	  
    XCloseDisplay(dpy);
    longjmp(ioerrorbuf, 1);
}
#endif /* CHAINSAW */

GLOBAL void     doinput()
{
  char            sbuf[256];
  fd_set          ofdset, fdset;
  int             didevent = 1;
  long            elapsed;
  int             noplayer_updates = 0;
  int             sock = -1;
  int             si = 0;
  struct player  *player;

  signal(SIGALRM, setRedrawFlag);
  SetTimer();

#ifdef CHAINSAW
  signal(SIGPIPE, SIG_IGN);
#else
  signal(SIGPIPE, SIG_DFL);
#endif
  FD_ZERO(&ofdset);
  FD_ZERO(&fdset);
  playerchange = 1;
  nrobots = 0;

#ifdef CHAINSAW
#ifdef ibm
#define ibmKLUDGE (XIOErrorHandler)
#else
#define ibmKLUDGE
#endif
  XSetIOErrorHandler( ibmKLUDGE xtrekioerrorhandler);
#endif

  /* main loop */
  for (;;) {
    /* count players */

    nplayers = CountPlayers(nplayers);

    /* timeout if no players are active */
    if (sock < 0 && !nplayers && (noplayer_updates++ > (40 * UPS)))
      exit(0);

    if (!didevent) {
      int             nfound;

      fdset = ofdset;
      nfound = select(32, &fdset, (fd_set *) NULL, (fd_set *) NULL,
		      (struct timeval *) NULL);
      if (nfound < 0) {
	FD_CLR(xtrek_socket, &fdset);
	if (sock > 0)
	  FD_CLR(sock, &fdset);
      }
    }
    if (FD_ISSET(xtrek_socket, &fdset) || sock > 0 && FD_ISSET(sock, &fdset)) {
      struct sockaddr addr;
      int             addrlen = sizeof (addr);

      errno = 0;
      if (sock < 0) {
	sock = accept(xtrek_socket, &addr, &addrlen);
	if (sock > 0) {
	  static int      on = 1;

	  si = 0;
	  peerdied = 0;
	  signal(SIGPIPE, deadpeer);
	  ioctl(sock, FIONBIO, &on);
	  FD_SET(sock, &ofdset);
	}
      }
      errno = 0;

      if (sock > 0) {
	int             bytes_read = read(sock, &sbuf[si], sizeof sbuf - si);

	if (bytes_read <= 0)
	  peerdied = 1;
	else {
	  si += bytes_read;
	  if (si >= 2 && sbuf[si - 2] == '\015' && sbuf[si - 1] == '\012') {
	    char            display_name[256];
	    char            login[256];
	    char	    name[256];

	    sbuf[si - 2] = '\0';
	    if (sscanf(sbuf, "Display: %s Login: %s Name: %[^\r]", display_name, login, name) != 3) {
	      write(sock, "Bad format.\n", 12);
	      close(sock);
	      sock = -1;
	    }
	    else {
	      struct player  *player;

	      for (player = &players[0]; player < &players[MAXPLAYER]; player++)
		if (player->p_status == PFree) {
		  char            buf[256];

		  sprintf(buf, "Adding player %d on `%s'.\n",
			  player - players, display_name);
		  write(sock, buf, strlen(buf));
		  bzero(&player->p_stats, sizeof player->p_stats);
		  playerchange = 1;
		  nplayers++;
		  player->p_status = PSetup;
		  bzero(player->p_monitor,sizeof(player->p_monitor));
		  bzero(player->p_login,sizeof(player->p_login));
		  bzero(player->p_name,sizeof(player->p_name));
		  strncpy(player->p_monitor, display_name,
			  sizeof(player->p_monitor)-1);
		  strncpy(player->p_login, login,
			  sizeof(player->p_login)-1);
		  strncpy(player->p_name,name,
			  sizeof(player->p_name)-1);
		  goto slot_found;
		}
	      write(sock, "No more room in game.\n", 22);
	    }
	    close(sock);
	    sock = -1;
	slot_found:;
	  }
	}
      }
    }
    if (peerdied) {			/* bleah */
      peerdied = 0;
      playerchange = 1;
    }
    if (playerchange) {
      FD_ZERO(&ofdset);
      FD_SET(xtrek_socket, &ofdset);
      if (sock > 0)
	FD_SET(sock, &ofdset);
      nplayers = 0;
      nrobots = 0;
#ifdef CHAINSAW
      signal(SIGPIPE, SIG_IGN);
#else
      signal(SIGPIPE, SIG_DFL);
#endif
      for (player = &players[0]; player < &players[MAXPLAYER]; player++) {
	if (player->p_status != PFree) {
	  nplayers++;
	  if (!ISROBOT(player))
	    FD_SET(player->xcn, &ofdset);
	  else
	    nrobots++;
	}
      }
      if (nplayers > 0)
	noplayer_updates = 0;
      playerchange = 0;
    }
    while (do_the_redraw-- > 0)
      intrupt();

    didevent = 0;
    for (player = &players[0]; player < &players[MAXPLAYER]; player++) {
      XEvent          event;

#ifdef CHAINSAW
      ioerrorplayer = player;
      if (setjmp(ioerrorbuf)) {
	  player->display = 0;
	  player->p_flags = 0;
	  player->p_status = PFree;
	  playerchange = 1;
	  nplayers--;
	  continue;
      }
#endif /* CHAINSAW */

      switch (player->p_status) {
	case PFree:
	  continue;

	case PSetup:{
	    char           *rval = newwin(player);

	    playerchange = 1;
	    if (!rval)
	      player->p_mask = ALLEMPIRE;	/* unassigned initially */
	    else {
	      write(sock, rval, strlen(rval));
	      write(sock, "\n", 1);
	      player->p_status = PFree;
	      nplayers--;
	    }
	    close(sock);
	    sock = -1;
	    if (player->p_status == POutfit)	/* if status ok */
	      mapAll(player);
	  }
	  break;

	case POutfit:
	  if (!ISENTER(player)) {
	    SETENTER(player);
	    entrywindow(player);	/* show them the entry window */
	  }
	  else {
	    elapsed = time(0) - player->startTime;
	    /* exit point */
	    if (elapsed > AUTOQUIT) {
	      REMOVE_PLAYER(player);
	      continue;
	    }
	    else {
	      int             empire;

	      showTimeLeft(player, elapsed, AUTOQUIT, 0);
	      for (empire = 0; empire < numempires; empire++)
		redrawEmpire(player, empire, 0);
	    }
	  }
	  break;

	default:;
      }

      if (!FD_ISSET(player->xcn, &fdset) || ISROBOT(player))
	continue;
      if (!XPending(player->display))
	continue;
      didevent = 1;
      XNextEvent(player->display, &event);	/* grab the event */
      if ((!(player->copilot /* || watch */ )) &&
	  (player->p_updates > player->delay))
	CLRWAR(player);
      switch ((int) event.type) {
	case KeyPress:
	  if (player->p_status == POutfit) {
	    int             result = CheckSelectWindow(player, event.xkey.window);

	    if (result) {
	      playerchange = 1;
	      if (result == -1)
		REMOVE_PLAYER(player);
	      break;
	    }
	  }
	  if (inputIgnored(player))
	    break;
	  if ((ISSELFDEST(player)) /* && (!watch) */ ) {
	    warning(player, "Self destruct cancelled.");
	    CLRSELFDEST(player);
	  } {
	    char            buf[40];
	    int             nchar = XLookupString(&event.xkey, buf, 39, (KeySym *) NULL,
						  (XComposeStatus *) NULL);

	    if (nchar > 0) {
	      if (event.xkey.window == player->messagew)
		smessage(player, *buf);
	      else
		keyaction(player, *buf, &event.xkey);
	    }
	  }
	  break;

	case ButtonPress:
	  if (player->p_status == POutfit) {
	    int             result = CheckSelectWindow(player, event.xkey.window);

	    if (result) {
	      playerchange = 1;
	      if (result == -1)
		REMOVE_PLAYER(player);
	      break;
	    }
	  }
	  if (inputIgnored(player))
	    break;
	  if (ISSELFDEST(player) /* && (!watch) */ ) {
	    warning(player, "Self destruct cancelled.");
	    CLRSELFDEST(player);
	  } {
	    int             empire;

	    for (empire = 0; empire < numempires + 2; empire++)
	      if (event.xbutton.window == player->waremp[empire])
		goto war_button;
	  }
	  buttonaction(player, &event.xbutton);
	  break;
      war_button:
	  waraction(player, &event.xbutton);
	  break;

	case Expose:
	  {

	    Window          window = event.xexpose.window;

	    if (window == player->statwin && (ISSHOWSTATS(player)))
	      redrawStats(player, player->statwin);
	    else if (window == player->tstatw)
	       /* statline (player, 1) */ ;
	    else if (window == player->mapw)
	      player->redrawall = 1;
	    else if (window == player->iconWin)
	      drawIcon(player);
	    else if (window == player->helpWin)
	      fillhelp(player);
	    else if (window == player->playerw)
	      playerlist(player);
	    else if (window == player->planetw)
	      planetlist(player);
	    else if (player->p_status == POutfit) {
	      if (window == player->w)
		showMotd(player);
	      if (window == player->qwin) {
		redrawQuit(player, player->qwin);
		elapsed = time(0) - player->startTime;
		showTimeLeft(player, elapsed, AUTOQUIT, 1);
	      }
	      else {
		int             empire;

		for (empire = 0; empire < numempires; empire++)
		  if (window == player->win[empire])
		    redrawEmpire(player, empire, 1);
	      }
	    }
	  }
	  break;
	default:;			/* NOTE what should be default? */

      }					/* switch */
    }
  }					/* (infinite) loop */
}

GLOBAL void     initinput(p)
  register struct player *p;
{
  Display        *display = p->display;
  int             empire;

  XSelectInput(display, p->iconWin, ExposureMask);
  XSelectInput(display, p->w, INITKEYMASKS);
  XSelectInput(display, p->mapw, INITKEYMASKS);
  XSelectInput(display, p->messagew, INITKEYMASKS);

  XSelectInput(display, p->tstatw, ExposureMask);

  /* May want this for a later refresh XSelectInput(display,p->war INITMASKS); */
  XSelectInput(display, p->helpWin, ExposureMask);
  XSelectInput(display, p->planetw, ExposureMask);
  XSelectInput(display, p->playerw, ExposureMask);

  for (empire = 0; empire < numempires + 2; empire++)
    XSelectInput(display, p->waremp[empire], INITMASKS);
}
