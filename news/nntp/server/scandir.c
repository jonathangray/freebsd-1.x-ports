#ifndef lint
static char	*sccsid = "@(#)$Header: /a/cvs/386BSD/ports/news/nntp/server/scandir.c,v 1.1 1993/07/19 20:04:32 nate Exp $";
#endif

#include "common.h"

/*
 * scan_dir -- scan the current directory for news articles,
 *	loading the article numbers into art_array.  Return
 *	number of articles loaded.
 *
 *	Parameters:	"low_msg", "high_msg" are the low
 *			and high messages numbers in this
 *			group; we ignore numbers outside this
 *			range.
 *
 *	Returns:	Number of articles loaded into
 *			array.
 *
 *	Side effects:	Changes "art_array".
 */

extern	int	intcmp();
extern char *malloc(), *realloc();

scan_dir(low_msg, high_msg)
int	low_msg, high_msg;
{
	register struct direct	*dirent;
	register DIR		*dirp;
	int			artnum;

	num_arts = 0;

	dirp = opendir(".");

	if (dirp == NULL)
		return (0);

	while ((dirent = readdir(dirp)) != NULL) {
		artnum = atoi(dirent->d_name);
#ifdef DYNAMIC_ART_ARRAY
		if (artnum == 0 || artnum < low_msg || artnum > high_msg)
			continue;
		/* Expand/allocate art_array elements as necessary */
		if (num_arts + 1 >= size_art_array) {
			size_art_array += 1024;
			if (art_array) {
#ifdef SYSLOG
				syslog(LOG_INFO,
				    "increasing art_array to %d elements",
				    size_art_array);
#endif
				art_array = (int *)realloc(art_array,
				    size_art_array * sizeof(*art_array));
			} else
				art_array = (int *)
				    malloc(size_art_array * sizeof(*art_array));
			if (art_array == 0) {
#ifdef SYSLOG
				syslog(LOG_ERR,
				    "scan_dir(): malloc/realloc failed");
#endif
				num_arts = 0;
				size_art_array = 0;
				size_art_array = 0;
				closedir(dirp);
				return(0);
			}
		}
		art_array[num_arts] = artnum;
 		++num_arts;
#else
		if (artnum != 0 && artnum >= low_msg && artnum <= high_msg)
			art_array[num_arts++] = artnum;
#endif

	}
	closedir(dirp);

	qsort((char *) art_array, num_arts, sizeof(int), intcmp);

	return (num_arts);
}


/*
 * intcmp -- compare to integers.
 *
 *	Parameters:	"x", "y" point to the integers to be compared.
 *
 *	Returns:	-1 if "x" is less than "y",
 *			0 if "x" equals "y", and
 *			1 if "x" is greater than "y".
 *
 *	Side effects:	None.
 */

intcmp(x, y)
register int	*x, *y;
{
	return (*x - *y);
}
