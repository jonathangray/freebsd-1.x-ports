/*
 * Copyright (c) 1989 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char copyright[] = "Copyright (c) 1990 Regents of the University of California.\nAll rights reserved.\n";
static char SccsId[] = "@(#)pop_pass.c  1.7 7/13/90";
#endif not lint

#include <stdio.h>
#include <sys/types.h>
#include <strings.h>
#include <pwd.h>
#include "popper.h"

/* 
 *  pass:   Obtain the user password from a POP client
 */

int pop_pass (p)
POP     *   p;
{
    register struct passwd  *   pw;
    char *crypt();

    /*  Look for the user in the password file */
    if ((pw = getpwnam(p->user)) == NULL)
        return (pop_msg(p,POP_FAILURE,
            "Password supplied for \"%s\" is incorrect.",p->user));

    /*  We don't accept connections from users with null passwords */
    if (pw->pw_passwd == NULL)
        return (pop_msg(p,POP_FAILURE,
            "Password supplied for \"%s\" is incorrect.",p->user));

    /*  Compare the supplied password with the password file entry */
    if (strcmp (crypt (p->pop_parm[1], pw->pw_passwd), pw->pw_passwd) != 0)
        return (pop_msg(p,POP_FAILURE,
            "Password supplied for \"%s\" is incorrect.",p->user));

    /*  Build the name of the user's maildrop */
    (void)sprintf(p->drop_name,"%s/%s",POP_MAILDIR,p->user);

    /*  Make a temporary copy of the user's maildrop */
    if (pop_dropcopy(p) != POP_SUCCESS) return (POP_FAILURE);

    /*  Set the group and user id */
    (void)setgid(pw->pw_gid);
    (void)setuid(pw->pw_uid);
#ifdef DEBUG
    if(p->debug)pop_log(p,POP_DEBUG,"uid = %d, gid = %d",getuid(),getgid());
#endif DEBUG

    /*  Get information about the maildrop */
    if (pop_dropinfo(p) != POP_SUCCESS) return(POP_FAILURE);

    /*  Initialize the last-message-accessed number */
    p->last_msg = 0;

    /*  Authorization completed successfully */
    return (pop_msg (p,POP_SUCCESS,
        "%s has %d message(s) (%d octets).",
            p->user,p->msg_count,p->drop_size));
}
