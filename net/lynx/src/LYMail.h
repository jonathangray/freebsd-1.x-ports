

#ifndef LYMail_H
#define LYMail_H

#ifndef LYSTRUCTS_H
#include "LYStructs.h"
#endif

extern void mailmsg PARAMS((int cur, char *owner_address, 
                			char *filename, char *linkname));
extern void reply_by_mail PARAMS((char *mail_address, char *filename));


#endif /* LYMail_H */

