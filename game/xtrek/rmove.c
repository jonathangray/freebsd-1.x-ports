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

/*
#include <X11/Xlib.h>
*/

#include <stdio.h>

#include "defs.h"
#include "data.h"
#include "polar.h"

#define AVOID_TIME		4
#define SHOOTTIME 		7

#define STAY	(1 << 0)
#define RUN	(1 << 1)
#define ATTACK  (1 << 2)
#define REPAIR  (1 << 3)
#define AVOID	(1 << 4)

#define NORMALIZE(d) 		((unsigned char)((d)&0xff))

#define rstatus (p->p_rmode)
#define avoidTime (p->p_timer)


/*
 * calculate_hit: figure out what direction to aim to hit somebody. - works
 * with torps/ships/... - doesn't work with torps anymore since some of their
 * speed depends on the speed and direction of the ship that fires them.  Oh
 * well.
 *
 * We'll call the robot's current position A the enemy's current position B, and
 * the place where the torpedo hits C.  T will be the time at which the
 * torpedo and enemy both reach C.  We'll call the internal angle in ABC at A
 * alpha, at B beta and at C gamma.  a and b are the speeds of the robot and
 * enemy, and c is the distance between them.
 *
 * To solve the problem we'll use the first compute beta as the difference
 * between the enemy's current direction (beta1) and the direction from the
 * enemy to the robot (beta2).  Now we can use the law of sines to compute
 * alpha from sin(alpha) / b*T = sin(beta) / a*T since the T's (which we
 * don't yet know!) cancel
 *
 * This gives delta = pi - alpha - beta since alpha, beta and delta are the
 * three angles of a triangle.  Now we apply the law of sines again to
 * compute T from sin(alpha) / b*T = sin(delta) / d
 *
 * Since we now know the time to collision and we know the speed and direction
 * of the enemy it is easy to compute where the enemy will be when the
 * collision takes place (C).  Knowing C and A it is trivial to compute the
 * angle between them.
 *
 * We'll return a 0 if a hit is impossible and a 1 if it is possible. Note that
 * some `possible' hits will take place a long time in the future.
 *
 * Sorry about the ridiculous number of arguments.  Most of them are pretty
 * obvious with the exception that the speeds used by this routine must be
 * multiplied by the constant WARP1 by the calling procedure. John Riedl
 */
/****************************************************************
  
  /-------------\
  /		\
  /		 \
  /			  \
  |	  XXXX	   XXXX	  |
  |	  XXXX	   XXXX	  |
  |	  XXX	    XXX	  |
  \		X	  /
  --\     XXX     /--
  | |    XXX    | |
  | |	      | |
  | I I I I I I I |
  |  I I I I I I	|
  \	       /
  --	     --
  \-------/
  XXX			   XXX
  XXXXX		  XXXXX
  XXXXXXXXX	     XXXXXXXXXX
  XXXXX	  XXXXX
  XXXXXXX
  XXXXX	  XXXXX
  XXXXXXXXX	     XXXXXXXXXX
  XXXXX		  XXXXX
  XXX			   XXX
  
  **************
  *  BEWARE!!  *
  **************
  
  All ye who enter here:
  Most of the code in this module
  is twisted beyond belief!
  
  Tread carefully.
  
  If you think you understand it,
  You Don't,
  So Look Again.
  
  ****************************************************************/
/*
 * (The above was taken from the emacs display code by James Gosling) but it
 * applies here as well.......
 */

/*
 * An aside from the UBC part of the team... if the code is twisted then it's
 * not easily extendable.  We're working on untwisting the spaghetti....
 * Mike
 */

#define MINSPEED .59
#define MINANGLE 3

#define FLIP(x) {if (((u_char)x) > 128) (x) = 256 - ((u_char)x);}
#define ACUTE(x) { if (((u_char)x) > (128-((u_char)x))) (x) = (128-(u_char)(x)); }
#define CORRECT(x) \
{ if ((x) > 128) (x) -=256; \
else if ((x) < -128) (x) +=256; }

LOCAL int       calculate_hit(me_speed, me_x, me_y, enemy_speed, enemy_dir, enemy_x,
			      enemy_y, pimpact_dir, pimpact_time, pimpact_dist, pimpact_x, pimpact_y)
int             me_speed;
int             me_x, me_y;
int             enemy_speed;
int             enemy_x, enemy_y;
unsigned char   enemy_dir;
int            *pimpact_dir, *pimpact_x, *pimpact_y, *pimpact_dist;
double         *pimpact_time;
{
    unsigned char   beta1;
    unsigned char   beta2;
    unsigned char   alpha, beta, delta;
    double          asin_input;
    int             impact_dist;
    int             impact_time;
    int             impact_x;
    int             impact_y;
    int             impact_dir;
    u_char            difang;
    
    beta1 = enemy_dir;
    beta2 = POLAR_DIRECTION(me_x - enemy_x, me_y - enemy_y);
    beta = beta1 - beta2;
    FLIP(beta);
    
    asin_input = POLAR_SINE(beta) * enemy_speed / (me_speed + 0.01);
    if (asin_input > 1 || asin_input < -1)/* No way to hit */
	return 0;
    alpha = POLAR_ARCSINE(asin_input);
    FLIP(alpha);
    
    /* Choose the more acute of the solution angles (quicker hit) */
    ACUTE(alpha);
    delta = 128 - alpha - beta;
    alpha = alpha ? alpha : 1;
    delta = delta ? delta : 1;
    /* calculate the time to the impact */
    impact_dist = POLAR_DISTANCE(enemy_x - me_x, enemy_y - me_y);
    
    difang = NORMALIZE(beta2 - enemy_dir);
    ACUTE(difang); 
    if ((difang < MINANGLE) || (ABS((int)difang-128)<MINANGLE) ||
	(enemy_speed < MINSPEED)) {
	impact_x=enemy_x;
	impact_y=enemy_y;
	impact_dir = POLAR_DIRECTION(enemy_x - me_x, enemy_y - me_y);
	impact_time = (float) (impact_dist / (me_speed - enemy_speed + 0.01));
	} else {
	    
	    impact_time = (POLAR_SINE(alpha) * impact_dist) /
		((POLAR_SINE(delta) * enemy_speed) + 0.01);
	    /* calculate the location of the impact */
	    impact_x = POLAR_X(enemy_speed * impact_time, enemy_dir) + enemy_x;
	    impact_y = POLAR_Y(enemy_speed * impact_time, enemy_dir) + enemy_y;
	    impact_dir = POLAR_DIRECTION(impact_x - me_x, impact_y - me_y);
	}
    if (debug)
	printf("Aim in direction %d to hit in time %lf at x %d  y %d\n",
	       impact_dir, impact_time, impact_x, impact_y);
    
    *pimpact_dist = impact_dist;
    *pimpact_time = impact_time;
    *pimpact_x = impact_x;
    *pimpact_y = impact_y;
    *pimpact_dir = impact_dir;
    
    return 1;
}

/*
 * Stupid Sun 3.2 tan routine calls matherr and *prints* a message if it
 * returns something other that 1.
 */
int             matherr()
{
    return 1;
}

/*
 * This function will send the robot back to the center of it's space when it
 * has nothing better to do.  Centers are defined in the empire definition
 */

LOCAL void      go_home(p)
aPlayer        *p;
{
    
#ifdef never
    int             x, y;
    
    x = empires[p->p_empire].centerx;
    y = empires[p->p_empire].centery;
    if ((ABS(x - p->p_x) < ORBDIST) && (ABS(y - p->p_y) < ORBDIST))
	set_speed(p, ORBSPEED);
    else {
#endif
	
	SETPLLOCK(p);
	p->p_planet = p->p_empire * 10;
	set_speed(p, p->ship.cruise);
	
#ifdef never
	p->p_desdir = rgetcourse(p, x, y);
    }
#endif
}

#define AVOIDDISTP 3500
#define AVOIDDISTM 3000
#define IMPACTTIME 5

LOCAL void      AvoidMinesAndPlanetsAndTorps(p)
aPlayer        *p;
{
    int             mx, my, tx, ty, dx, dy, courseto, coursedif, difs = 0, difave = 0, i, dist;
    struct torp    *t;
    struct planet  *pl;
    int             tcourse, impact_dist, impact_x, impact_y, thits = 0, future_x, future_y;
    double          impact_time;
    u_char           ldir;
    
    for (i = MAXPLAYER * MAXTORP, t = &torps[MAXPLAYER * MAXTORP - 1]; i; i--, t--) {
	if((t->t_status==TFree) || (t->t_mask & p->p_mask)) continue;
	tx = t->t_x;
	ty = t->t_y;
	mx = p->p_x;
	my = p->p_y;
	dx = tx - mx;
	dx = ABS(dx);
	dist = (t->t_status == TMove) ? 10000 : t->t_range * 3;
	if (dx > dist)
	    continue;
	dy = ty - my;
	dy = ABS(dy);
	if (dy > dist)
	    continue;
	if ((dx * dx + dy * dy) > dist * dist)
	    continue;
	courseto = newcourse(p, tx, ty);
	coursedif = courseto - p->p_dir;
	CORRECT(coursedif);
	
	
	if (ABS(coursedif) < 64 && t->t_status == TMine) {
	    difs++;
	    difave += coursedif;
	}
	if ((t->t_status == TMine) )
	    continue;
	(void) calculate_hit
	    (WARP1(t->t_speed), t->t_x, t->t_y,
	     WARP1(p->p_speed), p->p_dir, p->p_x, p->p_y,
	     &tcourse, &impact_time, &impact_dist,
	     &impact_x, &impact_y);
	future_x = POLAR_X(p->p_speed * impact_time, p->p_dir) + p->p_x;
	future_y = POLAR_Y(p->p_speed * impact_time, p->p_dir) + p->p_y;
	if ((ABS(impact_x - future_x) < t->t_range) &&
	    (ABS(impact_y - future_y) < t->t_range)) {
	    thits++;
	    ldir = POLAR_DIRECTION(t->t_x - p->p_x, t->t_x - p->p_y);
	    }
    }
    
    if (thits) {
	p->p_desdir = NORMALIZE(ldir + 64);
	shield_up(p);
	detothers(p);
	SETWEPREL(p);
    }
    for (i = MAXPLANETS, pl = &planets[MAXPLANETS - 1]; i; i--, pl--) {
	if ((pl->pl_owner == p->p_empire) && !ISRHOSTILE(p))
	    continue;
	tx = pl->pl_x;
	ty = pl->pl_y;
	mx = p->p_x;
	my = p->p_y;
	dx = tx - mx;
	dy = ty - my;
	if (OUTRANGE(dx, dy, AVOIDDISTP))
	    continue;
	courseto = newcourse(p, tx, ty);
	coursedif = courseto - p->p_dir;
	CORRECT(coursedif);
	
	if (ABS(coursedif) < 64) {
	    difs++;
	    difave += coursedif;
	}
    }
    if (!difs)
	difs = 1;
    difave /= difs;
    if (difave < 0) {
	if (difave < 0) {
	    p->p_desdir = NORMALIZE(p->p_dir + 64 + difave + thits);
	}
	else {
	    p->p_desdir = NORMALIZE(p->p_dir - 64 + difave + thits);
	}
    }
}

GLOBAL void     startrobot(empire, flags, x, y)
int             empire;
int             flags, x, y;
{
    register aPlayer *p;
    int             rpno = 0, tmp, cnt = 0;
    int             mask = FLAG(empire);
    
    for (tmp = MAXPLAYER, p = &players[MAXPLAYER - 1]; tmp--; p--) {
	if (p->p_status == PFree) {
	    cnt++;
	    rpno = tmp;
	}
    }
    if (cnt < MAXPLAYER / 2)
	return;
    p = &players[rpno];
    bzero(p, sizeof (struct player));
    nplayers++;
    enter(empire, "", rpno);
    SETROBOT(p);
    SETCOPILOT(p);
    if (RAND(100) < p->ship.sneaky) {
	SETSNEAKY(p);
    }
    else {
	CLRSNEAKY(p);
    }
    strncpy(p->p_login, "Robot", 6);
    strncpy(p->p_name, empires[empire].robotname,
	    strlen(empires[empire].robotname) + 1);
    /* Set robot difficulty */
    p->p_flags |= flags;
    if (ISRHOSTILE(p)) {
	p->p_hostile |= mask;
    }
    SETPATROL(p);
    if (ISASSIST(p)) {
	p->p_desx = x;
	p->p_desy = y;
    }
}

#define SHOOTTEMP (TEMPSCALE(90))

LOCAL void      shoot(p, ecourse, tcourse, torp_can_hit, edist,
		      edamage, lv, imptime)
aPlayer        *p;
u_char         ecourse;
int            tcourse, torp_can_hit, edist, edamage, lv, imptime;
{
    int             burst,fburst;
    burst = p->ship.burst;
    /*
      if (imptime < 5)
      imptime = -1;
      */
    if ((p->p_wtemp > SHOOTTEMP) && random() & 1)
	return;
    
    /** level 1 **/
    if (edist <= ((3 * p->ship.phaserdist) / 4) && p->p_fuel >= COST(p, Phaser)) {
	if (ISCLOAK(p))
	    cloak_off(p);
	phaser(p, ecourse);
    }
    if ((13000 < edist) || torp_tubes[p->p_no])
	return;
    /** level 1 **/
    if (torp_can_hit && p->p_ntorp < MAXTORP &&
	p->p_fuel >= COST(p, Torp)) {
	if (ISCLOAK(p))
	    cloak_off(p);
	if (ISKILLER(p)) {
	    fburst=(p->p_fuel/(COST(p,Torp)+1));
	    if(fburst<burst) burst=fburst;
	    fburst=MAXTORP-p->p_ntorp;
	    if(fburst<burst) burst=fburst;
	    if(burst>4) burst=4;
	    switch (burst) {
	      case 4:
		ntorp(p, NORMALIZE(tcourse - 5), TMove, imptime);
	      case 3:
		ntorp(p, NORMALIZE(tcourse + 4), TMove, imptime);
	      case 2:
		ntorp(p, NORMALIZE(tcourse - 3), TMove, imptime);
	      case 1:
		ntorp(p, NORMALIZE(tcourse + 2), TMove, imptime);
	      case 0:
	      default:
		ntorp(p, tcourse, TMove, imptime);
	    }
	}
	else {
	    long timptime=imptime + (edist >> 11);
	    if(timptime>TFIREVAR+TFIREMIN) timptime=TFIREVAR+TFIREMIN;
	    /* ntorp(p, NORMALIZE(tcourse + (RAND((edist >> 10)+ 1) -(edist >> 11))), TMove, imptime + (edist >> 11)); */
	    ntorp(p, (unsigned char)(NORMALIZE(tcourse)+((random()%9)-3)), TMove, (int)(imptime + random()%5-2));
	}
	}
}



#define ENGAGEDIST 60000
#define OTHERDIST 8000
#define OTHERDISTSQR (8000 * 8000)

LOCAL void      findenemy(p, edist, ecourse)
aPlayer        *p;
int            *edist;
u_char         *ecourse;
{
    
    aPlayer        *enemy;
    register int    i;
    register aPlayer *j;
    int             torp_can_hit = 0;
    double          dx, dy, impact_time, tmpdist;
    int             tdist;
    int             p_nonhostile = !ISRHOSTILE(p);
    int             tmpx, tmpy, tmpdir, tmpspeed, tmpimptime = 10;
    int             impact_dist;
    int             impact_x, impact_y;
    int             tcourse, cdist, angadd;
    u_long           p_agressive;
    
    /* p_nonhostile = 0; */
    /* run robots */
    p_agressive = p_nonhostile ? (p->p_swar | p->p_hostile) : ALLEMPIRE;
    
    enemy = p;
    *edist = GWIDTH;
    /* Find an enemy */
    for (i = MAXPLAYER, j = &players[MAXPLAYER - 1]; i--; j--) {
	if (j->p_status != PAlive || j == p)
	    continue;
	if (p_agressive & j->p_mask || (j->p_swar | j->p_hostile) & p->p_mask) {
	    if (debug)
		fprintf(stderr, "%d) found enemy %d in our space at %d,%d\n",
			p->p_no,
			j->p_no,
			j->p_x,
			j->p_y);
	    /* We have an enemy */
	    /* Get his range */
	    dx = j->p_x - p->p_x;
	    dy = j->p_y - p->p_y;
	    if (ISCLOAK(j)) {
		tmpdist = (dx * dx + dy * dy);
		if (dx>OTHERDIST || dx<-OTHERDIST || dy>OTHERDIST || dy<-OTHERDIST || tmpdist > OTHERDISTSQR || (random() % OTHERDISTSQR) < tmpdist) {
		    j->p_lv++;
		}
		else {
		    j->p_lv = 0;
		    j->p_lvdir = enemy->p_dir;
		    j->p_lvspeed = enemy->p_speed;
		    j->p_lvx = enemy->p_x;
		    j->p_lvy = enemy->p_y;
		}
	    }
	    else {
		j->p_lv = 0;
		j->p_lvdir = j->p_dir;
		j->p_lvspeed = j->p_speed;
		j->p_lvx = j->p_x;
		j->p_lvy = j->p_y;
	    }
	    
	    /* No way this enemy can be closer than known enemy */
	    if (dx > *edist || dy > *edist)
		continue;
	    tdist = POLAR_DISTANCE(dx, dy);
	    if (tdist < *edist) {
		if ((j->p_damage + p->ship.shotdamage < j->ship.maxdamage)
		    || ISROBOT(j) || ISWEPREL(j)) {
		    enemy = j;
		    *edist = tdist;
		    continue;
		    }
	    }
	}
    }
    /** level 1 **/
    if (enemy == p) {
	if (!ISRSTICKY(p) || (nplayers - nrobots) <= 0) {
	    /* No more enemies */
	    if (p->p_explode == 0)
		p->p_explode = p->p_updates + RGIVEUPTIME;
	    if (p->p_updates >= p->p_explode) {
		playerchange = 1;
		p->p_status = PFree;
		nplayers--;
		nrobots--;
		p->p_flags = 0;
		return;
	    }
	}
	else
	    p->p_explode = 0;
	go_home(p);
	return;
    }
    if (ISSNEAKY(p)) {
	if (*edist < p->ship.maxcloak && !ISCLOAK(p) &&
	    !ISWEPREL(p) && !(*edist < p->ship.mincloak)) {
	    cloak_on(p);
	    }
	else {
	    if (*edist < p->ship.mincloak) {
		SETWEPREL(p);
	    }
	}
    }
    else {
	if (*edist < p->ship.engage)
	    SETWEPREL(p);
    }
    if (*edist > p->ship.disengage)
	CLRWEPREL(p);
    
    /** level 1 **/
    if (*edist > p->ship.disengage * 3) {
	if (p->p_damage > 0) {
	    repair(p);
	    if (debug)
		fprintf(stderr, "%d) repairing damage at %d\n",
			p->p_no,
			p->p_damage);
	}
	else {
	    rstatus = STAY;
	    go_home(p);
	}
	return;
    }
    /* Get course for torp to nearest enemy */
    if (enemy->p_lv && enemy->p_lv < SHOOTTIME) {
	tmpspeed = enemy->p_lvspeed;
	tmpdir = enemy->p_lvdir;
	tmpx = POLAR_X(tmpspeed * enemy->p_lv, tmpdir) + enemy->p_lvx;
	tmpy = POLAR_Y(tmpspeed * enemy->p_lv, tmpdir) + enemy->p_lvy;
    } else {
	tmpspeed = enemy->p_speed;
	tmpdir = enemy->p_dir;
	tmpx = enemy->p_x;
	tmpy = enemy->p_y;
    }
    if (enemy->p_lv < SHOOTTIME) {
	torp_can_hit = calculate_hit
	    (WARP1(p->ship.torpspeed), p->p_x, p->p_y,
	     WARP1(tmpspeed), tmpdir, tmpx, tmpy,
	     &tcourse, &impact_time, &impact_dist,
	     &impact_x, &impact_y);
	tmpimptime = (int) impact_time;
    }
    *ecourse = newcourse(p, tmpx, tmpy);
    
    if (rstatus == AVOID) {
	if (--avoidTime <= 0)
	    rstatus = ATTACK;
	if (debug)
	    fprintf(stderr, "avoiding: dir = %d\n", p->p_desdir);
    }
    else {
	rstatus = ATTACK;
	if (ISCLOAK(p) && ISWEPREL(p))
	    cloak_off(p);
	
	if (p->p_etemp > 750 || p->p_fuel < 1500)	/* time to chill a bit */
	    set_speed(p, p->ship.refresh);
	else if (p->p_desdir != p->p_dir)
	    set_speed(p, p->ship.bturn);
	else
	    set_speed(p, p->ship.battle);
	
	if (!ISSHIELD(p))
	    shield_up(p);
	
	if (!ISRHARD(p))
	    p->p_desdir = *ecourse;
	else {				/* try to go where he will be when we
					   catch with him */
	    int             can_hit;
	    int             impact_dir;

	    can_hit = calculate_hit
		(WARP1(p->p_speed), p->p_x, p->p_y,
		 WARP1(tmpspeed), tmpdir, tmpx, tmpy,
		 &impact_dir, &impact_time, &impact_dist,
		 &impact_x, &impact_y);
	    
	    if (can_hit)
		p->p_desdir = impact_dir;
	    else {
		p->p_desdir = *ecourse;		/* go straight at him */
	    }
	    /* printf("foo: %d ",p->p_empire); */
	    if (*edist < p->ship.disengage) {
		cdist = (ISSNEAKY(p) && !ISWEPREL(p)) ? p->ship.mincloak : p->ship.circledist;
		if (*edist < cdist) {
		    angadd = (64 * ((float) (cdist - *edist) / cdist));
		    p->p_desdir = NORMALIZE(impact_dir + 64 + angadd);
		}
		else {
		    angadd = (64 * ((float) *edist / cdist));
		    p->p_desdir = NORMALIZE(impact_dir + 128 - angadd);
		    
		}
	    }
	    /* printf("bar: %d ",p->p_empire); */
	}
    }
    if (debug) {
	if (torp_can_hit)
	    printf("Aim in dir %d to hit in time %d at adjusted x %d, y %d\n",
		   tcourse, i, impact_x, impact_y);
	else
	    printf("Can't hit\n");
    }
    if ((enemy->p_lv < SHOOTTIME) && ISWEPREL(p)) {
	shoot(p, *ecourse, tcourse, torp_can_hit, *edist, enemy->p_damage, enemy->p_lv, tmpimptime);
    }
}

#define MINEDIST 6000
#define RUNDAMAGE DAMSCALE(40)
#define RUNSHIELD DAMSCALE(40)
#define RUNFUEL 600
#define RUNWTEMP TEMPSCALE(90)
#define RUNFAIL(p) (FAILED(p, Torp) || FAILED(p, Shield) || FAILED(p, Phaser))
#define TOOHOT(x) ((x) > DAMSCALE(100))
#define HELPDAMAGE(x) (((x) * 4)/5)

LOCAL void      runaway(p, edist, ecourse)
aPlayer        *p;
int             edist;
u_char          ecourse;
{
    if (((p->p_damage > RUNDAMAGE && p->p_shield < RUNSHIELD) ||
	 p->p_fuel < RUNFUEL || RUNFAIL(p) || p->p_wtemp > RUNWTEMP)
	&& edist < WARP1(4000)) {
	/* Run away!! Run away!! */
	if (!ISCLOAK(p) && p->p_fuel > 1000 && cloak_on(p) && ISCLOAK(p))
	    set_speed(p, p->ship.cloaked);
	else
	    set_speed(p, p->ship.flee);
	if (ISSHIELD(p) && TOOHOT(p->p_etemp + p->p_wtemp))
	    shield_down(p);
	p->p_desdir = ecourse - 128;
	if (edist < MINEDIST)
	    ntorp(p, 0, TMine, -1);
	}
    if ((p->p_damage > HELPDAMAGE(p->ship.maxdamage)) && !ISCALLEDHELP(p)) {
	startrobot(p->p_empire, PFRHARD, p->p_x, p->p_y);
	SETCALLEDHELP(p);
    }
}

#define PATROLDIST (5000*5000)

LOCAL void      patrol(p)
aPlayer        *p;
{
    struct planet  *pl;
    int             plnum;
    double	    dy, dx;
    
    if (ISWEPREL(p) || !ISPATROL(p))
	return;
    
    if (!ISASSIST(p)) {
	while (p->p_despl == -1) {
	    plnum = (random() >> 8) % numplanets - 1;
	    pl = &planets[plnum];
	    if (pl->pl_owner == p->p_empire) {
		p->p_despl = plnum;
		p->p_desx = pl->pl_x;
		p->p_desy = pl->pl_y;
	    }
	}
    }
    dx = p->p_x - p->p_desx;
    dy = p->p_y - p->p_desy;
    if (!OUTRANGE(dx, dy, PATROLDIST)) {
	p->p_despl = -1;
	CLRASSIST(p);
	return;
    }
    p->p_desspeed = ISASSIST(p) ? p->ship.hscruise : p->ship.cruise;
    
    
    if (ISSHIELD(p))
	shield_down(p);
    p->p_desdir = newcourse(p, p->p_desx, p->p_desy);
}


GLOBAL void     rmove(p)
register aPlayer *p;
{
    unsigned char   ecourse;
    int             edist;
    
    findenemy(p, &edist, &ecourse);
    runaway(p, edist, ecourse);
    patrol(p);
    AvoidMinesAndPlanetsAndTorps(p);
}
