/*
 * Copyright 1993 The University of Newcastle upon Tyne
 * 
 * Permission to use, copy, modify and distribute this software and its
 * documentation for any purpose other than its commercial exploitation
 * is hereby granted without fee, provided that the above copyright
 * notice appear in all copies and that both that copyright notice and
 * this permission notice appear in supporting documentation, and that
 * the name of The University of Newcastle upon Tyne not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission. The University of
 * Newcastle upon Tyne makes no representations about the suitability of
 * this software for any purpose. It is provided "as is" without express
 * or implied warranty.
 * 
 * THE UNIVERSITY OF NEWCASTLE UPON TYNE DISCLAIMS ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF
 * NEWCASTLE UPON TYNE BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 * 
 * Author:  Jim Wight (j.k.wight@newcastle.ac.uk)
 *          Department of Computing Science
 *          University of Newcastle upon Tyne, UK
 */

#include <sys/types.h>
#ifdef NODIRENT
#include <sys/dir.h>
#define dirent direct
#else
#include <dirent.h>
#endif

/*
 * Hopefully one of these will work
 */
#ifndef DIRSIZ
#define DIRSIZ(dp) ((dp)->d_reclen)
/*
#define DIRSIZ(dp)  \
     (((sizeof (struct dirent) - (sizeof((dp)->d_name)+1) + ((dp)->d_namlen+1)) + 3) & ~3)
*/
/*
#define DIRSIZ(dp)  \
     (((sizeof (struct dirent) - (sizeof((dp)->d_name)+1) + (strlen((dp)->d_name)+1)) + 3) & ~3)
*/
#endif

int 
scandir(dirname, namelist, select, compar)
     char *dirname;
     struct dirent ***namelist;
     int (*select)(), (*compar)();
{
    struct dirent *dent, *rent, **list;
    int nitems, items = 0;
    DIR *dirp;

    if ( !(dirp = opendir(dirname)) )
    {
	return(-1);
    }

    while (readdir(dirp))
    {
	++items;
    }
    if (!(list = (struct dirent **) malloc(items * sizeof(struct dirent *))))
    {
	return(-1);
    }
    rewinddir(dirp);

    nitems = 0;
    for (nitems = 0;  nitems < items && (dent = readdir(dirp));  ++nitems) {
	if (select && !(*select)(dent))
	{
	    continue;
	}

	if (!(rent = (struct dirent *) malloc(DIRSIZ(dent))))
	{
	    return(-1);
	}
	memcpy((char *) rent, (char *) dent, DIRSIZ(dent)); 

	list[nitems] = rent;
    }
    closedir(dirp);

    if (nitems && compar)
    {
	qsort(list, nitems, sizeof(struct dirent *), compar);
    }
    *namelist = list;

    return(nitems);
}

int
alphasort(d1, d2)
     struct dirent **d1, **d2;
{
    return(strcmp((*d1)->d_name, (*d2)->d_name));
}
