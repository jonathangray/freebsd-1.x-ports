/*                     /Net/dxcern/userd/timbl/hypertext/WWW/Library/Implementation/HTTP.html
                                HYPERTEXT TRANFER PROTOCOL
                                             
 */
#ifndef HTTP_H
#define HTTP_H

#include "HTAccess.h"

GLOBALREF HTProtocol HTTP;

#define URL_GET_METHOD  1
#define URL_POST_METHOD 2

#ifdef UCX
#define bcopy(s, d, n) memcpy((d), (s), (n))
#endif

#endif /* HTTP_H */

/*

   end of HTTP module definition
   
    */
