/* Copyright (c) 1989 The Regents of the University of California. All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met: 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer. 2.
 * Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution. 3. All advertising
 * materials mentioning features or use of this software must display the
 * following acknowledgement: This product includes software developed by the
 * University of California, Berkeley and its contributors. 4. Neither the
 * name of the University nor the names of its contributors may be used to
 * endorse or promote products derived from this software without specific
 * prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * @(#)pathnames.h 5.2 (Berkeley) 6/1/90 */

#ifndef _PATH_FTPUSERS
#define _PATH_FTPUSERS  "/etc/ftpusers"
#endif
#ifndef _PATH_FTPACCESS
#define _PATH_FTPACCESS "/usr/local/etc/ftpaccess"
#endif
#ifndef _PATH_EXECPATH
#define _PATH_EXECPATH  "/bin/ftp-exec"
#endif
#ifndef _PATH_PIDNAMES
#define _PATH_PIDNAMES  "/usr/local/daemon/ftpd/ftp.pids-%s"
#endif
#ifndef _PATH_CVT
#define _PATH_CVT       "/usr/local/etc/ftpconversions"
#endif
#ifndef _PATH_XFERLOG
#define _PATH_XFERLOG   "/usr/adm/xferlog"
#endif
#ifndef _PATH_PRIVATE
#define _PATH_PRIVATE   "/etc/ftpgroups"
#endif

#ifndef _PATH_UTMP
#define _PATH_UTMP      "/etc/utmp"
#define _PATH_WTMP      "/usr/adm/wtmp"
#define _PATH_LASTLOG   "/usr/adm/lastlog"
#endif

#ifndef _PATH_BSHELL
#define _PATH_BSHELL    "/bin/sh"
#endif
#ifndef _PATH_DEVNULL
#define _PATH_DEVNULL   "/dev/null"
#endif

#ifdef  HOST_ACCESS
#ifndef _PATH_FTPHOSTS
#define _PATH_FTPHOSTS  "/usr/local/etc/ftphosts"
#endif
#endif

