/*                                                               FTP access module for libwww
                                   FTP ACCESS FUNCTIONS
                                             
   This isn't really  a valid protocol module -- it is lumped together with HTFile . That
   could be changed easily.
   
   Author: Tim Berners-Lee. Public Domain. Please mail changes to timbl@info.cern.ch
   
 */
#ifndef HTFTP_H
#define HTFTP_H

#include "HTUtils.h"
#include "HTAnchor.h"
#include "HTStream.h"
#include "HTParse.h"

#define FILE_BY_NAME 0 
#define FILE_BY_TYPE 1
#define FILE_BY_SIZE 2
#define FILE_BY_DATE 3
extern BOOLEAN HTfileSortMethod;  /* specifies the method of sorting */

/*

Retrieve File from Server

  ON EXIT,
  
  returns                 Socket number for file if good.<0 if bad.
                         
 */
extern int HTFTPLoad PARAMS
((
  CONST char *          name,
  HTParentAnchor *      anchor,
  HTFormat              format_out,
  HTStream*             sink
));


/*

Return Host Name

 */
extern CONST char * HTHostName NOPARAMS;

#endif

/*

   end  */
