*** addng.old.c	Sat Jun  3 05:56:19 1989
--- addng.c	Sat Jun  3 05:56:20 1989
***************
*** 1,4
! /* $Header: /a/cvs/386BSD/ports/news/cnews/contrib/rn.mod/cdiff.addng.c,v 1.1 1993/08/27 02:46:50 alm Exp $
   *
   *	UW Mods:
   *		GETNG_YNQ - typing Q gets you out of the newsgroup loop

--- 1,4 -----
! /* $Header: /a/cvs/386BSD/ports/news/cnews/contrib/rn.mod/cdiff.addng.c,v 1.1 1993/08/27 02:46:50 alm Exp $
   *
   *	UW Mods:
   *		GETNG_YNQ - typing Q gets you out of the newsgroup loop
***************
*** 3,10
   *	UW Mods:
   *		GETNG_YNQ - typing Q gets you out of the newsgroup loop
   * $Log: cdiff.addng.c,v $
   * Revision 1.1  1993/08/27 02:46:50  alm
   * Initial revision
   *
!  * Revision 1.1  89/06/03  05:29:01  geoff
!  * Initial revision
   * 
   * Revision 1.3  87/09/10  19:39:47  sahayman
   * newlist for SERVER now asks for all groups from the dawn of time

--- 3,10 -----
   *	UW Mods:
   *		GETNG_YNQ - typing Q gets you out of the newsgroup loop
   * $Log: cdiff.addng.c,v $
   * Revision 1.1  1993/08/27 02:46:50  alm
   * Initial revision
   *
!  * Revision 1.2  89/06/03  05:35:30  geoff
!  * nuke a useless stat.
   * 
   * Revision 1.3  87/09/10  19:39:47  sahayman
   * newlist for SERVER now asks for all groups from the dawn of time
***************
*** 183,189
  char *ngnam;
  ART_NUM ngsize;
  {
-     char tst[128];
      long time();
   
      sprintf(tst, ngsize ? "%s/%s/1" : "%s/%s" ,spool,getngdir(ngnam));

--- 183,188 -----
  char *ngnam;
  ART_NUM ngsize;
  {
      long time();
   
      /*
***************
*** 186,192
      char tst[128];
      long time();
   
-     sprintf(tst, ngsize ? "%s/%s/1" : "%s/%s" ,spool,getngdir(ngnam));
      /*
       * After a long oddyssey of changes, we have
       * restored the patch 19 behaviour here.

--- 185,190 -----
  {
      long time();
   
      /*
       * After a long oddyssey of changes, we have
       * restored the patch 19 behaviour here.
***************
*** 192,197
       * restored the patch 19 behaviour here.
       * This gives the minimum number of wrong answers.
       * ..sah 87/07/29
       */
  
      if (stat(tst,&filestat) < 0)

--- 190,197 -----
       * restored the patch 19 behaviour here.
       * This gives the minimum number of wrong answers.
       * ..sah 87/07/29
+      * Bzzt!  Wrong!  Article 1 will always be long gone; nuke da stat(2).
+      * - geoff
       */
      /* not there, assume something good */
      return (ngsize ? 0L : time(Null(long *)));
***************
*** 193,204
       * This gives the minimum number of wrong answers.
       * ..sah 87/07/29
       */
! 
!     if (stat(tst,&filestat) < 0)
! 	return (ngsize ? 0L : time(Null(long *)));
! 	/* not there, assume something good */
!     else
! 	return filestat.st_mtime;
  }
  
  bool

--- 193,200 -----
       * Bzzt!  Wrong!  Article 1 will always be long gone; nuke da stat(2).
       * - geoff
       */
!     /* not there, assume something good */
!     return (ngsize ? 0L : time(Null(long *)));
  }
  
  bool



