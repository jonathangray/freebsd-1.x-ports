/*
 * Default place for the socket
 */
#define SOCKNAME	"/usr/lib/news/nntp_msgid"
#define PIDFILE         "/usr/lib/news/msgidd.pid"


#ifdef NEEDMSGS
/*
 * Message types from client to server
 */
char *msgs[] = {
    "add-",
    "can-",
    "hst-",
    "old-"
};
#endif

/* 
 * Messages to clinet side called with (MUST BE IN SAME ORDER AS msgs[])
 */
#define MADD	0
#define	MCANCEL	1
#define	MHOST	2
#define MOLD	3
