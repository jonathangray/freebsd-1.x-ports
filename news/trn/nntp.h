/* $Id: nntp.h,v 1.5 1994/02/22 01:49:05 nate Exp $
*/ 
/* The authors make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

#ifdef USE_NNTP

bool	nntp_group _((char*,NG_NUM));
bool	nntp_stat _((ART_NUM));
bool	nntp_header _((ART_NUM));
FILE	*nntp_body _((ART_NUM));
time_t	nntp_time _((void));
bool	nntp_newgroups _((time_t));
bool	nntp_listgroup _((void));
char	*nntp_get_a_line _((char*, int));
char	*nntp_artname _((void));
char	nntp_handle_timeout _((bool_int));
void	nntp_cleanup _((void));

#ifdef USE_XTHREAD
long	nntp_readcheck _((void));
long	nntp_read _((char*,long));
#endif

#include "nntpclient.h"

#endif /* USE_NNTP */
