/*	$Header: /a/cvs/386BSD/ports/comm/flexfax/faxd/Getty.h,v 1.1 1993/08/31 23:42:38 ljo Exp $
/*
 * Copyright (c) 1990, 1991, 1992, 1993 Sam Leffler
 * Copyright (c) 1991, 1992, 1993 Silicon Graphics, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and 
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the names of
 * Sam Leffler and Silicon Graphics may not be used in any advertising or
 * publicity relating to the software without the specific, prior written
 * permission of Sam Leffler and Silicon Graphics.
 * 
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
 * 
 * IN NO EVENT SHALL SAM LEFFLER OR SILICON GRAPHICS BE LIABLE FOR
 * ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
 * OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
 * LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 */
#ifndef _GETTY_
#define	_GETTY_

/*
 * Getty support base class.  Support for
 * System V, BSD, etc. are derived from this. 
 */
#include "Str.h"

const int GETTY_MAXARGS		= 64;	// max args passed to getty

struct termios;

class Getty {
private:
    pid_t	pid;			// pid of getty/login process
    fxBool	nologin;		// if true, check for nologin file
    u_int	timeout;		// if nonzero, timeout login attempts
    fxStr	line;			// device name
    fxStr	speed;			// line speed
    const char* argv[GETTY_MAXARGS];	// argv passed to getty
    fxStr	argbuf;			// stash for argv strings

    void setupArgv(const char* args);
protected:
    Getty(const fxStr& line, const fxStr& speed,
	u_int timeout = 60, fxBool nologin = TRUE);

    static const fxStr getty;		// getty program

    virtual void setupSession(int modemFd) = 0;

    virtual void error(const char* fmt, ...);
    virtual void fatal(const char* fmt, ...);
public:
    virtual ~Getty();

    virtual void run(int fd, const char* args);
    virtual fxBool wait(int& status, fxBool block = FALSE) = 0;
    virtual void hangup();

    pid_t getPID();
    void setPID(pid_t);

    const char* getLine();

    void setNologin(fxBool);
    void setTimeout(u_int);
};
inline pid_t Getty::getPID()		{ return pid; }
inline void Getty::setPID(pid_t p)	{ pid = p; }
inline const char* Getty::getLine()	{ return line; }

extern Getty* OSnewGetty(const fxStr& dev, const fxStr& speed);
#endif /* _GETTY_ */
