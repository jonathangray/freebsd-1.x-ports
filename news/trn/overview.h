/* $Id: overview.h,v 1.3 1993/11/17 23:03:37 nate Exp $
*/
/* The authors make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

/* The order of the overview file fields */
#define OV_NUM  	0
#define OV_SUBJ 	1
#define OV_FROM 	2
#define OV_DATE 	3
#define OV_MSGID	4
#define OV_REFS 	5
#define OV_BYTES	6
#define OV_LINES	7
#define OV_OTHERS	8	/* this catch-all field must be last */
#undef	OV_OTHERS_HAS_XREFS	/* forces trn to assume xrefs always exist */

/* If ALL the overview files trn will be accessing have xrefs in the OTHERS
** field you may choose to define OV_OTHERS_HAS_XREFS.  This may save some
** article accessing during the time that it would take trn to determine
** this for itself. */

/* NOTE that you must NOT define OV_XREFS unless you have opted to create
** a new (non-standard) overview field for xrefs instead of placing them
** in the OTHERS field.  If you have included the xref header without its
** prefix into a field of its own, define OV_XREFS with the appropriate
** field number in the list above.  If it has the "Xref:" prefix and you
** didn't want to point the OV_OTHERS field at it (for some reason) you
** can define both OV_XREFS and OV_LAX_XREFS and trn accepts a field with
** or without the "Xref:" prefix. */

#undef	OV_XREFS	/* only define when using non-standard .overview */
#undef	OV_LAX_XREFS	/* allow xref field to have a header-prefix */

/* What name to append to the directory name to read an overview file.
** This REQUIRES a leading slash unless you're getting fancy.
*/
#define OV_FILE_NAME	"/.overview"

/* How many overview lines to read with one NNTP call */
#define OV_CHUNK_SIZE	100
