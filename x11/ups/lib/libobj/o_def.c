/* o_def.c - routines to define objects, fields etc */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char obj_def_sccsid[] = "@(#)o_def.c	1.12 26/7/92 (UKC)";


#include <local/wn.h>

#include <string.h>

#include <local/ukcprog.h>
#include <mtrprog/alloc.h>

#include "obj.h"
#include "o_priv.h"
#include "o_mkobj.h"

static struct fldlnst *new_fldln PROTO((void));
static struct fdescst *new_fdesc PROTO((void));
static int uatoi PROTO((const char **p_s, int def, int scale));
static const char *default_get_name PROTO((objid_t code));
static int default_dump PROTO((char *arg, objid_t code, int level));

/*  horizontal and vertical scaling factors for the 'c' format size
 *  modifier - see define_objtype below
 */
static int Xscale = 1, Yscale = 1;

/*  Default width and depth for a field - see comments
 *  in define_objtype below.
 *  These values are multiplied by Xscale (Yscale) to get a default
 *  width (depth) in pixels.
 */
#define DEF_WIDTH 10
#define DEF_DEPTH 1

/*  Set the scaling factors Xscale and Yscale (see above).
 *  Should be called before using the 'c' scaler in define_objtype,
 *  either explicitly or implicitly.
 *  The scale factors are used implicitly when using the default width or
 *  depth for a field, or using a string literal in a field.
 *
 *  Xscale and Yscale are also used in the display of string literals.
 *  This means that at present the only sensible thing to do is to
 *  call this routine with the width and depth of the font that
 *  literals are to be drawn in.
 */
void
set_field_scale_factors(x, y)
int x, y;
{
	Xscale = x;
	Yscale = y;
}

/*  max length of a literal (of the form [literal] in a format string
 */
#define MAX_LITERAL 50

/*  allocation routines for structs fldlnst and fdescst - see alloc.h
 */
ALLOC_NEW(static,struct fldlnst,fldln,fl_next)
ALLOC_NEW(static,struct fdescst,fdesc,fd_next)

/*  Define type type. Type should be a small integer. After this call,
 *  it can be used as an argument to new_object() and change_type().
 *  The type of an object determines its width, depth, indentation
 *  of its children on the display, selectability, and what function
 *  (if any) to call when it is freed.
 *
 *  The arguments are:
 *
 *  type:	number of this type (see above)
 *  format:	defines the number, sizes and layout of the fields
 *		of objects of this type
 *  child_indent:
 *		how much further in pixels to indent the left margin
 *		when displaying children of objects of this type
 *  select:	function to be called when an object of this type is selected
 *		and (*can_select)() returns true. Called as
 *
 *		(*select)(wn,code,x,y,width,depth,flags)
 *		window_t wn;
 *		objid_t code;
 *		int x, y, width, depth, flags;
 *
 *		code is the code of the selected object,
 *		x,y is its origin in pixels window wn
 *		width, depth are its width and depth in pixels
 *		flags is a set of bit valued flags SEL_* defined in iface.h
 *  can_select: function to be called in the style
 *		"(*can_select)(code) objid_t code" when the user tries
 *		to select object code of this type. Should return TRUE
 *		if the object can be selected.
 *  free_obj:	if not NULL, this function is called in the style
 *		"(*free_obj)(code) objid_t code" just object code
 *		of this type is freed.
 *  getsize:	if not NULL, this function is called in the style
 *		
 *		(*getsize)(code,par,sz)
 *		objid_t code, par;
 *		struct szst *sz;
 *
 *		by the obj package to determine the size of an object
 *		(code) when the display is reformatted. par is the
 *		code of the object's parent.
 *		sz is a pointer to a structure whose fields should be
 *		filled in as follows:
 *		sz_width, sz_depth: the width and depth of the object.
 *		sz_same: set to 1 if all this objects siblings are the
 *		         same size.
 *
 *  format is a character string defining the fields of this object.
 *  the syntax of format is
 *
 *	format = ['\n'] format1
 *	format1 = fieldspec | fieldspec format1
 *	fieldspec = fieldspec1 ['\n']
 *	fieldspec1 = literal | varspec
 *	literal = char | char literal
 *	varspec = '%' ['*'] ['[' userinfo ']'] [size]['.' size] typechar
 *	size = number ['c']
 *	number = digit | digit number
 *	digit = <0-9>
 * 
 *  A format string is similar to a printf format string. Each
 *  varspec corresponds in sequence with a field of the object.
 *  On the display, fields are lined up left to right. A newline
 *  character in the format string causes a new line of fields
 *  to be started. In turn, objects are lined up several to a line,
 *  but only as many as will fit are fitted on a line.
 *  An initial newline in the format string means always display
 *  this object on a new line. A newline at the end of the format
 *  string means make this the last object on the line.
 *  The format character typechar selects the routines used to
 *  display and edit this field - for their meanings see
 *  define_format below.
 */
void
define_objtype(type, format, child_indent, select, can_select, free_obj, getsize, getcolor)
int type;
const char *format;
int child_indent;
obj_select_func_t select;
obj_can_select_func_t can_select;
obj_free_obj_func_t free_obj;
obj_get_size_func_t getsize;
obj_get_color_func_t getcolor;
{
	struct fldlnst *start_fldln, *prev_fldln, *fldln;
	struct fdescst *start_fdesc, *prev_fdesc, *fdesc;
	struct odescst *p_odesc;
	char *user_info, *cptr, buf[MAX_LITERAL+1], *lim;
	int lwidth, ldepth, width, depth, obj_width, obj_depth;
	int fnum, fchar, newline, i, flags, is_literal;
	
	if (type < 0 || type >= MAX_OBJTYPES)
		panic("type out of range in define_objtype");

	/*  initialise various counters and sizes
	 */
	obj_width = obj_depth = 0;
	lwidth = ldepth = fnum = 0;
	prev_fdesc = fdesc = NULL;
	prev_fldln = fldln = NULL;
	
	/*  deal with initial newline
	 */
	if (*format == '\n') {
		flags = OB_NL_BEFORE;
		format++;
	}
	else
		flags = 0;
	

	lim = buf + MAX_LITERAL;

	/*  gcc complains if the following two lines are missing.
	 *  I'm not sure whether they are actually necessary - just
	 *  now I don't feel inspired to analyse the code.
	 */
	start_fldln = NULL;
	start_fdesc = NULL;

	/*  this loop goes round once for each fieldspec. Literal strings
	 *  are treated as a varspec with no corresponding field.
	 */
	while (*format != '\0' && *format != '\n') {
		if (*format == '%' && format[1] != '%') {
			/* process a varspec
			 */
			format++;
			fchar = 0;
			/* '*' means varspec with no field
			 */
			if (is_literal = *format == '*')
				format++;
			
			/*  process optional '[<userinfo>] string
			 */
			if (*format == '[') {
				cptr = buf;
				format++;	/* skip '[' */
				while (*format != ']') {
					if (cptr > lim)
						panic("uinfo overflow");
					*cptr++ = *format++;
				}
				*cptr = '\0';
				user_info = strsave(buf);
				format++;	/* skip ']' */
			}
			else	
				user_info = NULL;

			/* get the width, depth and typechar
		 	 */
			width = uatoi(&format, DEF_WIDTH * Xscale, Xscale);
			if (*format == '.')
				format++;
			depth = uatoi(&format, DEF_DEPTH * Yscale, Yscale);
			fchar = *format++;
		}
		else {
			/*  process a literal - just a sequence
			 *  characters. The literal is stored in the
			 *  userinfo field with the reserved format
			 *  character '*'
			 */
			cptr = buf;
			while (*format != '\0' && *format != '\n' &&
				       (*format != '%' || format[1] == '%')) {
				if (*format == '%')
					format++;
				if (cptr > lim)
					panic("literal overflow");
				*cptr++ = *format++;
			}
			*cptr = '\0';
			user_info = strsave(buf);
			width = strlen(buf) * Xscale;
			depth = Yscale;
			is_literal = TRUE;
			fchar = '*';
		}

		/* keep track of the depth of this line of fields
		 */
		if (depth > ldepth)
			ldepth = depth;
		
		/*  link in a new fdesc structure - prev_fdesc will be
		 *  NULL for the start of a line of fields
		 */
		fdesc = new_fdesc();
		if (prev_fdesc != NULL)
			prev_fdesc->fd_next = fdesc;
		else
			start_fdesc = fdesc;
		prev_fdesc = fdesc;
		
		/*  set the fields for this fdesc.
		 */
		fdesc->fd_width = width;
		fdesc->fd_fnum = (is_literal) ? -1 : fnum++;
		fdesc->fd_user_info = user_info;
		
		/*  look up the typechar functions in the formats table
		 *  and set the edit and draw routines accordingly
		 */
		for(i = 0; i < Num_formats && Formats[i].fo_fch != fchar; i++)
			;
		if (i == Num_formats)
			panic("unknown format");
		fdesc->fd_edit = Formats[i].fo_edit;
		fdesc->fd_draw = Formats[i].fo_draw;
		fdesc->fd_getwidth = Formats[i].fo_getwidth;
		lwidth += width;
		
		/*  if at the end of a line or the end of the format string,
		 *  get a fldlnst to head this line and set it from
		 *  the width, depth etc. Keep the total object width
		 *  and depth up to date
		 */
		if ( (newline = *format == '\n') || *format == '\0') {
			fldln = new_fldln();
			if (prev_fldln != NULL)
				prev_fldln->fl_next = fldln;
			else
				start_fldln = fldln;
			prev_fldln = fldln;
			fldln->fl_fdesc = start_fdesc;
			fdesc->fd_next = NULL;
			prev_fdesc = NULL;
			fldln->fl_ldepth = ldepth;
			if (newline && format[1] != '\0')
				format++;
			if (lwidth > obj_width)
				obj_width = lwidth;
			obj_depth += ldepth;
			lwidth = ldepth = 0;
		}
	}
	fldln->fl_next = NULL;
	if (*format == '\n')
		flags |= OB_NL_AFTER;
	if (getsize == NULL)
		flags |= OB_FIXED_SIZE;
	if (fnum == 1)
		flags |= OB_ONE_FVAL;
	/*  set the fields of this entry in the format table
	 */
	p_odesc = Odesc + type;
	p_odesc->od_width = obj_width;
	p_odesc->od_depth = obj_depth;
	p_odesc->od_fldln = start_fldln;
	p_odesc->od_child_indent = child_indent;
	p_odesc->od_can_select = can_select;
	p_odesc->od_select = select;
	p_odesc->od_free_obj = free_obj;
	p_odesc->od_getsize = getsize;
	p_odesc->od_get_color = getcolor;
	p_odesc->od_get_name = default_get_name;
	p_odesc->od_dump_obj = default_dump;
	p_odesc->od_nfields = fnum;
	p_odesc->od_flags = flags;
}

/*  Routine to deal with optional ascii field sizes. *p_s (note
 *  double indirection) is a pointer to a string containing an
 *  optional ascii decimal number, followed by an optional scale
 *  character. If the number is there, read it, otherwise use the
 *  supplied default def. If there is a scale character 'c', multiply
 *  the result by scale. Move *p_s past any read characters.
 */
static int
uatoi(p_s, def, scale)
const char **p_s;
int def, scale;
{
	const char *s;
	int val;
	
	for (val = 0, s = *p_s; *s >= '0' && *s <= '9'; s++)
		val = val * 10 + *s - '0';
	if (s == *p_s)
		val = def;
	else {
		if (*s == 'c') {
			s++;
			val *= scale;
		}
	}
	*p_s = s;
	return val;
}

void
set_objtype_get_name_func(type, func)
int type;
obj_get_name_func_t func;
{
	if (type < 0 || type >= MAX_OBJTYPES)
		panic("type out of range in sognf");
	
	Odesc[type].od_get_name = func;
}

void
set_objtype_dump_func(type, func)
int type;
obj_dump_func_t func;
{
	if (type < 0 || type >= MAX_OBJTYPES)
		panic("type out of range in sodf");
	
	Odesc[type].od_dump_obj = func;
}

static const char *
default_get_name(code)
objid_t code;
{
	return (const char *)code;
}

static int
default_dump(arg, code, level)
char *arg;
objid_t code;
int level;
{
	panic("no dump defined for this object type");
	return -1;	/* to satisfy gcc */
}

/*  Default draw routine for the 's' format character - can be
 *  changed by a call to define_format. Draw dets->dr_fval at
 *  dets->dr_x,dets->dr_y in dets->dr_wn. If dets->dr_user_info is
 *  non-null, left justify, else right justify.
 *
 *  At present, this right-justifies using the length of the string
 *  multiplied by Xscale. This is bad because it depends on Xscale
 *  being the width of a character at the time of the call.
 */
void
s_draw(dets)
register struct drawst *dets;
{
	int x;
	
	x = dets->dr_x;
	if (dets->dr_user_info == NULL || dets->dr_user_info[0] != '-')
		x += dets->dr_width - strlen((char *)dets->dr_fval) * Xscale;
	if (dets->dr_fval != NULL)
		wn_ttext(dets->dr_wn, (char *)dets->dr_fval, x, dets->dr_y,
							dets->dr_fg, dets->dr_bg);
}

/*  Draw routine for the '*' format character. This char is treated
 *  specially - it is used for drawing literals. Draw the literal
 *  dets->dr_user_info at dets,dr_[xy] in window dets->dr_wn.
 */
void
l_draw(dets)
register struct drawst *dets;
{
	if (*dets->dr_user_info != ' ' || dets->dr_user_info[1] != '\0')
		wn_ttext(dets->dr_wn, dets->dr_user_info, dets->dr_x, dets->dr_y,
							dets->dr_fg, dets->dr_bg);
}

/*  Define a format character (typechar). Means that from now on
 *  (*f_edit)() will be called to edit fields with this fchar,
 *  and (*f_draw)() called to draw it. define_format for a character
 *  must be called *before* using the field character in define_objtype().
 *  Changes made after calling define_objtype() will have no effect.
 *  The predefined function no_edit() should be given as the editing
 *  function for non editable fields.
 */
void
define_vsformat(fch, f_edit, f_draw, f_getwidth)
int fch;
obj_edit_func_t f_edit;
obj_draw_func_t f_draw;
obj_getwidth_func_t f_getwidth;
{
	int i;
	
	for (i = 0; i < Num_formats; i++)
		if (Formats[i].fo_fch == fch)
			panic("format already defined");
	if (i == Num_formats && Num_formats >= MAX_FORMATS)
		panic("too many formats");
	Formats[i].fo_fch = fch;
	Formats[i].fo_edit = f_edit;
	Formats[i].fo_draw = f_draw;
	Formats[i].fo_getwidth = f_getwidth;
	Num_formats++;
}
