/* -*-C-*- dumpchar.h */
/*-->dumpchar*/
/**********************************************************************/
/****************************** dumpchar ******************************/
/**********************************************************************/

void
dumpchar(tcharptr,s)			/* DEBUGGING routine */
register struct char_entry *tcharptr;
char s[];
{
    UNSIGN16 ilimit;
    register UNSIGN16 i;
    register COORDINATE j;
    register *raster_word;

    raster_word = tcharptr->rasters;	/* pointer to first raster word */
    (void)fprintf(stderr,
        "DumpChar(%s): pfrp = %d\trasters at %06o",
	s,tcharptr->pfrp,raster_word);
    NEWLINE(stderr);
    (void)fprintf(stderr,"\t(xcp,ycp) = %d %d",xcp,ycp);
    NEWLINE(stderr);
    (void)fprintf(stderr,"\t(wp,hp) = %d %d",tcharptr->wp,tcharptr->hp);
    NEWLINE(stderr);
    ilimit = (tcharptr->wp + 31)/32;
    for (j = tcharptr->hp; j > 0; --j)
    {
        for (i = 0; i < ilimit; ++i)
	    (void)fprintf(stderr,"%9x ",*raster_word++);
	(void)fprintf(stderr,"| ");
    }
    NEWLINE(stderr);
}
