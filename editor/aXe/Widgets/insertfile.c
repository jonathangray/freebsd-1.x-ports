/* InsertFileNamed is taken from .../mit/lib/Xaw/TextPop.c */ 
/***********************************************************
Copyright 1989 by the Massachusetts Institute of Technology,
Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/
#include <X11/Intrinsic.h>
#include <X11/Xaw/Text.h>
#include <fcntl.h>
#include <stdio.h>

Boolean
InsertFileNamed(tw, str)
Widget tw;
char *str;
{
  int fid;
  XawTextBlock text;
  char buf[BUFSIZ];
  XawTextPosition start_pos, pos;

  if ( (str == NULL) || (strlen(str) == 0) ||
       ((fid = open(str, O_RDONLY)) <= 0))
    return(FALSE);

  start_pos = pos = XawTextGetInsertionPoint(tw);
  text.firstPos = 0;
  text.format = FMT8BIT;

  while ((text.length = read(fid, buf, BUFSIZ)) > 0) {
    text.ptr = buf;
    if (XawTextReplace(tw, pos, pos, &text) != XawEditDone) {
      /*
       * If the replace failed then remove what we have
       * replaced so far, and return an error.
       */
      text.length = 0;
      (void) XawTextReplace(tw, start_pos, pos, &text);
      (void) close(fid);
      return(FALSE);
    }
    pos += text.length;
  }
  (void) close(fid);
  XawTextSetInsertionPoint(tw, pos);
  return(TRUE);
}
