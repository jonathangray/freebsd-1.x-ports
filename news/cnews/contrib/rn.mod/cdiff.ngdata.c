*** ngdata.old.c	Sat Jun  3 05:56:22 1989
--- ngdata.c	Sat Jun  3 05:56:22 1989
***************
*** 1,4
! /* $Header: /a/cvs/386BSD/ports/news/cnews/contrib/rn.mod/cdiff.ngdata.c,v 1.1 1993/08/27 02:46:50 alm Exp $
   *
   * Modified to work with NNTP server.  -- Phil Lapsley
   * $Log: cdiff.ngdata.c,v $
   * Revision 1.1  1993/08/27 02:46:50  alm
   * Initial revision
   *

--- 1,4 -----
! /* $Header: /a/cvs/386BSD/ports/news/cnews/contrib/rn.mod/cdiff.ngdata.c,v 1.1 1993/08/27 02:46:50 alm Exp $
   *
   * Modified to work with NNTP server.  -- Phil Lapsley
   * $Log: cdiff.ngdata.c,v $
   * Revision 1.1  1993/08/27 02:46:50  alm
   * Initial revision
   *
***************
*** 2,9
   *
   * Modified to work with NNTP server.  -- Phil Lapsley
   * $Log: cdiff.ngdata.c,v $
   * Revision 1.1  1993/08/27 02:46:50  alm
   * Initial revision
   *
!  * Revision 1.1  89/06/03  05:44:45  geoff
!  * Initial revision
   * 
   * Revision 1.2  87/07/29  14:28:38  sahayman
   * SERVER changes merged in

--- 2,9 -----
   *
   * Modified to work with NNTP server.  -- Phil Lapsley
   * $Log: cdiff.ngdata.c,v $
   * Revision 1.1  1993/08/27 02:46:50  alm
   * Initial revision
   *
!  * Revision 1.2  89/06/03  05:55:42  geoff
!  * nuke da stats
   * 
   * Revision 1.2  87/07/29  14:28:38  sahayman
   * SERVER changes merged in
***************
*** 240,246
      register ART_NUM min = 1000000;
      register ART_NUM maybe;
      register char *p;
-     char tmpbuf[128];
      
      dirp = opendir(dirname);
      if (!dirp)

--- 240,245 -----
      register ART_NUM min = 1000000;
      register ART_NUM maybe;
      register char *p;
      
      dirp = opendir(dirname);
      if (!dirp)
***************
*** 250,263
  	    for (p = dp->d_name; *p; p++)
  		if (!isdigit(*p))
  		    goto nope;
! 	    if (*dirname == '.' && !dirname[1])
! 		stat(dp->d_name, &filestat);
! 	    else {
! 		sprintf(tmpbuf,"%s/%s",dirname,dp->d_name);
! 		stat(tmpbuf, &filestat);
! 	    }
! 	    if (! (filestat.st_mode & S_IFDIR))
! 		min = maybe;
  	}
        nope:
  	;

--- 249,264 -----
  	    for (p = dp->d_name; *p; p++)
  		if (!isdigit(*p))
  		    goto nope;
! 	    /*
! 	     * dp->d_name is all-numeric.  anyone dopey enough to create
! 	     * an all-numeric component of a newsgroup name should be chopped
! 	     * into tiny bits and the bits should be jumped on.  even given
! 	     * such dopes, rn should not attempt to read a directory later on,
! 	     * and it wouldn't be the end of the world if it did anyway.
! 	     * so i'll take the miniscule risk in return for getting to
! 	     * NUKE DA stat(2)s! - geoff
! 	     */
! 	    min = maybe;
  	}
        nope:
  	;
***************
*** 265,268
      closedir(dirp);
      return min==1000000 ? 0 : min;
  }
- 

--- 266,268 -----
      closedir(dirp);
      return min==1000000 ? 0 : min;
  }

