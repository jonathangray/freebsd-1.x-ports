/* -*-C-*- clrbmap.h */
/*-->clrbmap*/
/**********************************************************************/
/****************************** clrbmap *******************************/
/**********************************************************************/

void
clrbmap()

{
#if    SEGMEM
    register INT16 x,y;		/* loop indices */
    register UNSIGN32 *p;	/* bitmap pointer */

    for (y = 0; y < YBIT; ++y)
    {
        p = (UNSIGN32*)BITMAP(y,0);

#if    FASTZERO
	(void)zerom(p,(UNSIGN32)(XBIT));
#else
	for (x = 0; x < XBIT; (++p,++x))
	    *p = (UNSIGN32)0;
#endif

    }

#else /* NOT SEGMEM */
#if    FASTZERO

/***********************************************************************
Fast assembly  language clear  -- runtime  histogramming showed  24%  of
total time was spent  in this routine for  the large bitmap required  by
the Toshiba P1315 180 dpi dvi driver!
***********************************************************************/
    (void)zerom(bitmap,(UNSIGN32)(XBIT*YBIT));

#else /* NOT FASTZERO */


    register INT32 nword;	/* loop index */
    register UNSIGN32 *p;	/* bitmap pointer */

    p = (UNSIGN32*)BITMAP(YBIT-1,XBIT-1);	/* the last element */

    for (nword = (XBIT*YBIT); nword; (--nword,--p))
	*p = (UNSIGN32)0;

#endif /* FASTZERO */

#endif /* SEGMEM */
}

