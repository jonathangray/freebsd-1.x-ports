/* scansbr.h - definitions for scan () */
/* scansbr.h,v 1.1.1.1 1993/01/30 04:40:24 jtc Exp */

extern char *scanl;

#define	SCNENC	2		/* message just fine, but encrypted(!!) */
#define	SCNMSG	1		/* message just fine */
#define	SCNEOF	0		/* empty message */
#define	SCNERR	(-1)		/* error message */
#define	SCNNUM	(-2)		/* number out of range */
#define	SCNFAT	(-3)		/* fatal error */


#ifndef	UK
#define	FORMAT	\
"%4(msg)%<(cur)+%| %>%<{replied}-%?{encrypted}E%| %>\
%02(mon{date})/%02(mday{date})%<{date} %|*%>\
%<(mymbox{from})%<{to}To:%14(friendly{to})%>%>%<(zero)%17(friendly{from})%>  \
%{subject}%<{body}<<%{body}>>%>\n"
#else /* UK */
#define	FORMAT	\
"%4(msg)%<(cur)+%| %>%<{replied}-%?{encrypted}E%| %>\
%02(mday{date})/%02(mon{date})%<{date} %|*%>\
%<(mymbox{from})%<{to}To:%14(friendly{to})%>%>%<(zero)%17(friendly{from})%>  \
%{subject}%<{body}<<%{body}>>%>\n"
#endif /* UK */

#define	WIDTH	78

int	scan ();
