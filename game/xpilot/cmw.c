/* $Id: cmw.c,v 1.1 1994/02/23 14:40:04 jkh Exp $
 *
 * XPilot, a multiplayer gravity war game.  Copyright (C) 1991-93 by
 *
 *      Bjørn Stabell        (bjoerns@staff.cs.uit.no)
 *      Ken Ronny Schouten   (kenrsc@stud.cs.uit.no)
 *      Bert Gÿsbers         (bert@mc.bio.uva.nl)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/*
 * This module contains code specific to the Trusted Solaris CMW 1.0 system
 * (aka SunOS CMW 1.0) - if you don't know what that is, then you don't
 * need this code.
 */

#ifdef SUNCMW

#define SunOS_CMW /* needed by some included files */

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <cmw/priv.h>
#include <cmw/security.h>
#include <cmw/scremdb.h>
#include <cmw/sctndbd.h>

#include "cmw.h"

/* The privilege set for this process */
static priv_t cmw_privs[SEC_SPRIVVEC_SIZE];

/*
 * cmw_priv_init: initialise privileges for the Sun CMW system
 */
void
cmw_priv_init(void)
{
    /* set inheritable to null set */
    PRIV_EMPTY(cmw_privs);
    if ( setprocpriv(sizeof (priv_set_t), cmw_privs,
                     PRIV_INHERITABLE, PRIV_SET) == -1 )
    {
        perror("cmw: error setting inheritable privileges");
        exit(1);
    }

    /* get process's permitted privileges */
    if ( getprocpriv(sizeof (priv_set_t), cmw_privs, PRIV_PERMITTED) == -1 )
    {
        perror("cmw: error getting process privileges");
        exit(1);
    }

    /* set effective set equal to permitted, but without NET_ALLOWACCESS */
    PRIV_CLEAR(cmw_privs, PRIV_NET_ALLOWACCESS);
    if ( setprocpriv(sizeof (priv_set_t), cmw_privs,
                     PRIV_EFFECTIVE, PRIV_SET) == -1 )
    {
        perror("cmw: error setting effective privileges");
        exit(1);
    }

    return;
}

void
cmw_priv_assert_netaccess(void)
{
    PRIV_ASSERT(cmw_privs, PRIV_NET_ALLOWACCESS);
    if ( setprocpriv(sizeof (priv_set_t), cmw_privs,
                     PRIV_EFFECTIVE, PRIV_SET) == -1)
    {
        perror("cmw: error asserting permitted privileges");
        exit(1);
    }
    return;
}

void
cmw_priv_deassert_netaccess(void)
{
    PRIV_CLEAR(cmw_privs, PRIV_NET_ALLOWACCESS);
    if ( setprocpriv(sizeof (priv_set_t), cmw_privs,
                     PRIV_EFFECTIVE, PRIV_SET) == -1)
    {
        perror("cmw: error deasserting permitted privileges");
        exit(1);
    }
    return;
}

#endif /* SUNCMW */
