#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)xv_path_util.c 1.5 93/06/28";
#endif
#endif

/*
 *	(c) Copyright 1989 Sun Microsystems, Inc. Sun design patents 
 *	pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *	file for terms of the license.
 */

#include <string.h>
#include <errno.h>
#include <xview_private/xv_path_util.h>
#include <xview_private/portable.h>
#include <xview_private/i18n_impl.h>



/*
 * Private Utilties used by File Chooser packages
 */




Pkg_private void
#ifdef ANSI_FUNC_PROTO
xv_error_sprintf( Frame frame, int use_footer, char *format, ... )
#else
xv_error_sprintf(frame, use_footer, format,  va_alist)
     Frame frame;
     int use_footer;
     char *format;
va_dcl
#endif
{
    char buf[MAX_MSG_SIZE];
    AVLIST_DECL;
    va_list list;

    VA_START(list, format);
    (void) vsprintf(buf, format, list);
    va_end(list);

    window_bell( frame );
    if ( use_footer && (int)xv_get(frame, FRAME_SHOW_FOOTER) )
	xv_set(frame, FRAME_LEFT_FOOTER, buf, NULL);
} /* xv_error_sprintf() */




/*
 * Internal string copier.  Note, src or dest can be
 * NULL.  also note, dest is *replaced*, not copied into.
 */
Pkg_private char *
xv_strcpy ( dest, src )
     char *dest;
     char *src;
{
    char *new;

    if ( !src ) {
	xv_free_ref( dest );
	return dest;
    }

    new = xv_alloc_n(char, strlen(src)+1);

    if ( new ) {
	(void) strcpy(new, src);
	xv_free_ref( dest );
	dest = new;
    } else {
	(void) xv_error( NULL,
			ERROR_LAYER,	ERROR_SYSTEM,
			ERROR_STRING,	XV_MSG("out of memory, copying string!"),
			NULL );
    }
    return dest;
} /* xv_strcpy() */




/*
 * truncate last chunk of a path name, similar to the
 * dirname() function.
 */
Pkg_private void
xv_dirname( path )
     char *path;
{
    char *pos = strrchr( path, (int)'/' );

    if ( pos == path )
	path[1] = '\0';
    else if ( pos )
	*pos = '\0';
} /* xv_dirname() */




/*
 * concatenate a relative path to an absolute one.
 * don't worry about validation.
 */
Pkg_private char *
xv_dircat( path, dir )
     char *path;
     char *dir;
{
    char *new = xv_alloc_n(char, strlen(path) + strlen(dir) + 3);
    if ( !new )
	return (char *) NULL;

    if ( is_root(path) )
	/* don't worry about the 1 byte we don't use... */
	(void) sprintf( new, "/%s", dir );
    else if ( is_root(dir) )
	(void) sprintf( new, "%s/", path );
    else
	(void) sprintf( new, "%s/%s", path, dir );
    return new;
} /* xv_dircat() */




/*
 * determine if a given path is a directory.
 */
Pkg_private int
xv_isdir( path )
     char *path;
{
    struct stat sbuf;
    return (xv_stat(path, &sbuf) != -1) && S_ISDIR(sbuf.st_mode);
} /* xv_isdir() */



/*
 * Return the directory portion of a path name.
 * Memory is allocated.
 */
Pkg_private char *
xv_dirpart( path )
     char *path;
{
    char *dir = xv_strcpy(NULL, path);

    xv_dirname( dir );
    return dir;
} /* xv_dirpart() */



/*
 * Return file name portion of path.
 * ASSumes path is a full path to a file, not
 * a directory.  Memory is allocated.
 */
Pkg_private char *
xv_basepart( path )
     char *path;
{
    char *ptr = strrchr( path, (int)'/' );
    return xv_strcpy(NULL, ++ptr);
} /* xv_basepart() */



/*
 * Determine if file exists.
 */
Pkg_private int
xv_file_exists( path )
     char *path;
{
    struct stat stats;
    return (xv_stat(path, &stats) == 0);
} /* xv_file_exists() */




/*
 * Make stat call.  check errno to see if the reason
 * it failed is recoverable.
 */
Pkg_private int
xv_stat( path, stats )
     char *path;
     struct stat *stats;
{
    int status;

    while ( (status = stat(path, stats)) == -1 )
	if ( errno != EINTR )
	    break;

    return status;
} /* xv_stat() */




/*
 * Make access call.  check errno to see if the reason
 * it failed is recoverable.
 */
Pkg_private int
xv_access( path, mode )
     char *path;
     int mode;
{
    int status;

    while ( (status = access(path, mode)) == -1 )
	if ( errno != EINTR )
	    break;

    return status;
} /* xv_access() */





/*
 * Return expanded version of 'path'.
 * Allocates memory for return value.
 */
Pkg_private char *
xv_expand_path( path )
     char *path;
{
    char buf[MAXPATHLEN+1];

    expand_path(path, buf);

    /* BUG? expand_path() likes to leave a trailing '/' */
    if ( !is_root(buf) ) {
	size_t len = strlen(buf) - 1;
	if ( buf[len] == '/' )
	    buf[len] = '\0';
    }

    return xv_strcpy(NULL, buf);
} /* xv_expand_path() */
