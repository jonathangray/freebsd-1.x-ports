/*
**  BUF.H -- buffer definitions
**
**	Version:
**		@(#)buf.h	8.1	12/31/84
*/
#ifndef INGRES_MONITOR_BUF_H_
#define INGRES_MONITOR_BUF_H_

#define	BUFSIZE		256

typedef struct buf {
	struct buf	*nextb;
	char		buffer[BUFSIZE];
	char		*ptr;
} buf_t;

#endif /*  INGRES_MONITOR_BUF_H_ */
