.\" @(#)tech.ms	1.1 4/17/91 (UKC)
.\"
.\" Show a function name. Display the first argument in italics,
.\" followed by (), followed by the second argument if any in the
.\" previous font.
.de fn
\\$3\fI\\$1()\fP\\$2
..
.\"
.\" Show a variable name. This version just does it in italics
.de VA
\\$3\fI\\$1\fP\\$2
..
.TL
Implementation Notes for the Obj Library
.AU
Mark Russell
.AI
University of Kent at Canterbury
\*(DY
.NH 1
The Obj library interface.
.LP
This document describes the \fIObj\fP library.
This is the library used by \fIups\fP and \fIfs\fP to
display the tree of objects in their respective display areas.
In \fIfs\fP, the entire display area is a tree of one type
of object.
In \fIups\fP only part of the window is managed by the object
library.
Unlike \fIfs\fP, \fIups\fP uses many different types of objects.
.LP
This document is divided into two major sections.
The first gives a general overview of the design of the library.
The second gives a precise description of the application interface.
.NH 2
Overview
.LP
The Obj library handles the layout of a tree of objects on a bitmapped
display.
There are a number of application defined types of objects.
The attributes of an object type are the fields making up an object of the
type, and various user supplied functions to perform operations on objects
of the type.
Having defined object types, the application can create instances of
objects of the type.
These are inserted in the tree of objects.
There are facitilies to display the tree (using application-supplied
functions to display the fields of each object), and to allow the user point at
the display with a mouse and select groups of objects.
There is a function which the application calls to get a list of currently
selected objects.
The user can also edit the fields making up the objects.
The Obj routines determine which object and which field has been
selected, and it is up to the application to implement and interpret the
editing of the field.
.NH 3
Definition of object types
.LP
The application first defines one or more \fIobject types\fP.
This involves specifying the fields that make up an object of this type,
and functions to do various operations on the object.
Having defined an object type, instances of this type (the objects
themselves) can be created and deleted.
.NH 3
Fields within objects.
.LP
One of the attributes of an object type is the number and type of fields
making up the object.
This is specified with a \fIprintf\fP(3) like string, with escape sequences
for fields.
Some field types are predefined (e.g. `%s' for a string of text), but the
user can define others.
Defining a new field type involves specifying a character used to denote
it, a function to display it and a function to edit it.
The edit function is called when the user attempts to edit the field \(em
if the field is not editible a NULL pointer is given for this function.
The field need not be text \(em the drawing function can produce arbitrary
graphical output.
.NH 3
Handles on objects
.LP
Objects are identified by unique codes (called \fIobjcodes\fP),
supplied when the object is created.
These codes will usually be the address of a structure holding application
specific information about the object.
This scheme means that the application can go directly from an objcode to
its data referring to the object.
.NH 3
The tree of objects
.LP
The library maintains a singly rooted tree of objects, which defines the
display layout.
When the application creates an object it specifies where in the tree the
object should go.
There is also a function to move an object (and its children if any) from
one place in the tree to another.
.LP
There is a function to display the tree in a given region of a window from
a given offset, and also to scroll the display vertically by a given number of
pixels.
Displaying the tree involves displaying all the visible objects.
In turn, displaying an object is done by calling the user defined routines
to display each field of the object.
Objects are arranged in conceptual `lines', where the height of a line is
the height of the deepest object on the line.
Lines are filled with objects until the right hand margin is reached, or
until an object is encountered which the application has specified should
start a new line.
Objects can be fixed with, in which case all objects of a given type have
the same width, or variable width, in which case the application supplies a
function to calculate the width of an object.
.NH 3
User selection of objects
.LP
The facilities for user selection of objects and fields are at present
rather inflexible.
The application, on finding the mouse in the region used for the display of
objects calls a function which handles puck input until the puck leaves the
region.
If the user presses the left hand button over an object, that is considered
to be an attempt to \fIselect\fP that object.
The user supplied function for objects of the type selected is called to
see whether the object can be selected, and if so, another user supplied
function is called to highlight the object.
The user can also drag the mouse with the left button down to select
multiple objects.
.LP
The application can at any time get a list of the the currently selected
objects.
Normally the application applies any command the user asks for the the
currently selected list of objects.
.LP
There are also facitilies for the application to manipulate the selection
itself (by selecting or deselecting groups of objects).
.NH 3
User field editing
.LP
The pressing the middle button over an editable field is taken as a request
to edit that field.
If the application has defined that field as editable, the edit function
for a field of the type selected is called.
This function is supposed to handle mouse and keyboard input, and to
return when the edit is completed.
.NH 2
Interface
.LP
This section specifies the interface to the Obj library, defining 
each of the functions available to the application.
.NH 3
Defining formats and functions
.LP
.DS
void define_vsformat(fch, f_edit, f_draw, f_getwidth)
int fch;
void (*f_edit)(), (*f_draw)();
int (*f_getwidth)();

#define define_format(fch, f_edit, f_draw)
int fch;
void (*f_edit)(), (*f_draw)();
.DE
.LP
.fn define_vsformat
defines a format character
.VA fch .
This means that the three functions given will be used to edit, draw and
get the width of fields specified with the format character
.VA fch
in object type definitions.
.LP
.fn define_format
is a macro which calls
.fn define_vsformat
with a NULL pointer for the
.fn f_getwidth
argument.
.LP
The format character
.VA fch
can be any ASCII character except `*', '.', `[', ']', `%', `c' and the digits.
The
.VA f_edit
and
.VA f_draw
functions supplied are called as:
.DS
(*f_edit)(fdets)
struct drawst fdets;

(*f_draw)(p_fdets)
struct drawst *p_fdets;
.DE
Notice that
.VA f_edit
is passed the structure itself, whereas
.VA f_draw
gets a pointer to the structure.
This is because
.VA f_draw
is allowed to modify some fields of the structure \(em see below.
.LP
The structure
.VA drawst
is defined as:
.DS
struct drawst {
	int dr_wn;		/* window that coords are relative to */
	short dr_x;		/* origin ... */ 
	short dr_y;
	short dr_width;		/* ... and size of the field */
	short dr_depth;
	short dr_fnum;		/* field number within the parent object */
	objcode_t dr_code;	/* code of the parent object */
	fval_t dr_fval;		/* field value */
	char *dr_user_info;	/* userinfo from format string */
};
.DE
The fields of the structure have the following meanings:
.IP dr_wn
The window of this field.
This gives the window identifier of the window this field is drawn in and
offsets for the
.VA dr_x
and
.VA dr_y
values.
.IP "dr_x, dr_y"
The offset in pixels of the top left pixel of this field from the top left
corner of the window
.VA dr_wn .
.IP "dr_width, dr_depth"
The width and depth of this field in pixels.
The
.VA f_draw
function may modify the
.VA dr_width
field. This affects the horizontal position of any fields following this
one on this line of fields for this object only.
.IP dr_fnum
The field number, of this field within the parent object.
Field numbers start with zero for the top left field, and increase from
left to right and from top to bottom (i.e. in the same order as text is
read).
.IP dr_code
The objcode of the object that this field is a part of.
.IP
The value of this field, as supplied in
.fn set_field_value .
That is, the thing we should actually display.
.IP dr_user_info
User information supplied for this field when the object type for this
object was defined.
.LP
The reason for allowing
.VA f_draw
to alter
.VA dr_width
is to allow variable width fields to be drawn with constant spacing between
fields.
The
.VA f_draw
routine can set
.VA dr_width
depending on the width of the field it is drawing.
If, for example, the field is a string, it can find the length of the
string and use this to set
.VA dr_width .
.LP
If the
.VA f_getwidth
function is not NULL, it is called to find the size of a field as follows:
.DS
(*f_getwidth)(code,fnum,fval)
objcode_t code;
int fnum;
fval_t fval;
.DE
.LP
where
.VA code ,
.VA fnum
and
.VA fval
have the same meanings as the
.VA dr_code ,
.VA dr_fnum
and
.VA dr_fval
structure members defined above.
It should return the width in pixels of the field
.VA fval .
It is called when trying to determine which field a user has pointed at
with a mouse button press.
.NH 3
Defining new object types
.LP
.DS
void 
define_objtype(type,format,child_indent,select,can_select,free_obj,getsize)
int type;
char *format;
int child_indent;
void (*select)();
int (*can_select)();
void (*free_obj)(), (*getsize)();
.DE
This fundamental function defines an object type.
After this call objects of type
.VA type
can be created, displayed and manipulated.
The arguments are as follows:
.IP type
A small integer number naming this object type.
After this call, this number is used as the type argument to
.fn new_object
to create objects of this type.
.IP format
A string defining the number and layout of fields making up objects of this
type (details below).
.IP child_indent
How much further in pixels to indent the left margin for children of
objects of this type.
.IP can_select
function to be called when the user attempts to select an object of this
type.
Called as:
.DS
(*can_select)(code)
objcode_t code;
.DE
where
.VA code
is the code of the object the user attempted to select.
The function should return zero if the object cannot be selected, non zero
if it can.
.IP select
function to be called when an object of this type is selected and
.fn (*can_select)
returns non zero.
.IP free_obj
if not NULL, this function is called in the style
.DS
(*free_obj)(code)
objcode_t code;
.DE
when any object of this type is destroyed.
.VA code
is the code of the object being destroyed.
The function typically frees any resources used by the object with this
function.
.IP getsize
If NULL, objects of this type are taken to be a fixed size, calculated from
the number and size of the fields specified in the format string.
If non NULL, objects are assumed to be of variable size, and this function
is called for objects of this type to determine their individual sizes.
The function is called as:
.DS
struct szst {
	short sz_width, sz_depth;
	int sz_same;
};

(*getsize)(code,parcode,size)
objcode_t code, parcode;
struct szst *size;
.DE
The function should set size->sz_width and size->sz_depth to the width and
depth in pixels of object
.VA code .
In some cases, all children of a given parent will be the same size.
If this is the case
.VA size->sz_same
should be set to 1.
All further siblings of this object will then be assumed to have the size
returned for this object.
.VA parcode
is the code of the object's parent (supplied in case it is useful).
.NH 4
The format string
.LP
The
.VA format
parameter defines the fields making up an object of the given type.
Like a \fIprintf\fP(3) format string, it consists of literal characters and
escape sequences for fields.
The syntax of the format string is:
.DS
format = ['\n'] fieldspec [fieldspec ...] ['\n']
fieldspec = literal | varspec
literal = char [char ...]
varspec = '%' ['*'] ['[' userinfo ']'] [size ['.' size] typechar
size = number ['c']
.DE
Each \fIvarspec\fP describes a field of the object.
The only mandatory parts are the introductory `%' character and the
\fItypechar\fP, which is a single letter giving the type of the field.
This type may be one of the predefined types
or a type the user has defined using
.fn define_format
(see above).
.LP
The \fIvarspec\fP can contain optional extra fields, in the order they are
described below:
.IP `*'
this character immediately after the `%' means that this field has no
associated value.
It not assigned a field number and thus cannot be accessed by
.fn get_field_value
or
.fn set_field_value .
This is intended for fields where the type and size are sufficient
information for the draw routine.
.IP userinfo
a string between square brackets ('[' and ']') is stored and passed to the
draw and edit routines for this field (without the enclosing square
brackets), as the
.VA dr_userinfo
field of the
.VA drawst
structure.
.IP
This string allows a single drawing or editing routine to distinguish
different fields of a single object or fields in different object types.
It can also be used to control the behaviour of the drawing and editing
routines \(em for example, it is used to specify whether the default string
drawing routine should left or right justify the string in the field width.
.IP size
A single number or two numbers separated by a `.'
may precede the \fItypechar\fP, specifying the width
and depth of the field.
The default units are pixels, but if a size is followed by a `c' character
the number given is scaled by the scale factor for that dimension (see
.fn set_field_scale_factors
below.
There are default sizes for the width and depth of a field, but the
application will normally supply explicit sizes.
.IP
If the field is variable width (as specified in
.fn define_format
then the width supplied here is only used in calculating the width of the
object that this field forms a part of.
In this case the width given should probably be the maximum field width.
.LP
.DS
set_field_scale_factors(x,y)
int x,y;
.DE
This function sets the scaling to be applied when a `c' character is
appended to a field width
.VA x ) (
or depth
.VA y ) (
in an object format string.
The initial values are both one.
.LP
Unfortunately in the current version of the library, these scale factors
are used with string literals, so the only sensibly values for
.VA x
and
.VA y
are the width and depth of a character in the font used for the string
literals.
.NH 4
The
.fn (*select)
function
.LP
This function as mentioned above is called when the user selects an object
and the
.fn (*can_select)
function returns non zero.
It will normally highlight the object in some way if it is visible, and
perhaps record the selected/not selected state of the object.
Note that it is not necessary to keep a list of selected objects as their
is a function provided
.fn get_selection ) (
which returns this list.
.LP
The function is called as:
.DS
(*select)(wn,code,x,y,width,depth,flags)
int wn;
objcode_t code;
int x, y, width, depth, flags;
.DE
.VA code
is the code of the object to be selected.
.VA wn
is the window of the object.
.VA x ,
.VA y ,
.VA width
and
.VA depth
give the origin of the object in
.VA wn ,
and its size.
.VA flags
encodes information about the selection of this object which the function
can use to decide what action to take.
It is a bit string with the following bit values defined:
.IP SEL_ON
If set, the object is being selected, otherwise it's being deselected.
.IP SEL_WAS_ON
Set if the object was previously selected.
.IP SEL_VISIBLE
Set if the object is visible on the display.
No highlighting or unhighlighting should be done if this bit is not set.
.IP SEL_CHANGING
The selection state is changing from selected to deselected, or vice versa.
Not true, for example when an object is being highlighted because the
display is being repainted.
.LP
The next five flags are mutually exclusive and say why the
.fn (*select)
function is being called:
.IP SEL_CLEARING
The application called
.fn clear_selection
\&.
.IP SEL_SELECTING
The application called
.fn select_object
\&.
.IP SEL_UPDATING
The application turned updating on.
.IP SEL_PUCKSELECT
The user selected or deselected this object with the mouse.
.IP SEL_DISPLAYING
The Obj library is repainting the display.
.LP
In practice the
.fn (*select)
function for most object types can ignore most of these flags.
The basic rules are:
.IP
If you need to remember the selected/deselected state of an object, flip
this when SEL_CHANGING is set.
.IP
Only turn highlighting on and off if SEL_VISIBLE is set.
.LP
An application might also like to behave differently depending on  whether
objects are being selected/deselected by the user or by the application
itself \(em it can use the SEL_PUCKSELECT bit to achieve this.
.NH 3
Initialising the display
.LP
.DS
void set_obj_wn(wn)
int wn;
.DE
This function must be called once before any calls to create of display
objects.
It sets the window to be used for displaying the tree to
.VA wn .
The window gives the rectangular subregion of the window to be used for
the display, and an identifier for the window to use.
.LP
This function is also used to inform the Obj library that the size and
position of the display area has changed \(em it is called with
.VA wn
giving the new values.
This is typically used after a window size change.
.DS
init_objects()
.DE
Initialises the object tree.
This function must be called once only, and before any calls to
.fn new_object .
.NH 3
Batching changes to the display
.LP
Normally any changes to the object tree cause an immediate redraw of the
display area.
This would be very inefficient when making many changes, so there is a
function to batch updating of the display:
.DS
updating(val)
int val;
.DE
Turns the updating state off
.VA val "" (
== OBJ_UPDATING_OFF) or on
.VA val "" (
== OBJ_UPDATING_ON).
Returns the previous state of updating.
All functions in the Obj library can be called with updating off,
including selecting and deselecting objects.
In the case of selecting objects, the call to the application's object
selection routine is delayed until
updating is turned on again.
.LP
A typical use of this function is:
.DS
oldval = updating(OBJ_UPDATING_OFF);
< code doing lots of changes to the object tree>
(void) updating(oldval);
.DE
The previous state is kept in
.VA oldval
in this example as updating may already be off and we want to avoid
prematurely turning it on in this case.
.NH 3
Creating, moving and destroying objects
.LP
.DS
new_object(code,type,poscode,where)
objcode_t code;
int type;
objcode_t poscode;
int where;
.DE
Creates a new object with code
.VA code ,
of type
.VA type .
.VA type
must previously have been defined using
.fn define_objtype .
.VA poscode
is the code of a previously created object \(em it gives the position in
the tree where the new object should be put.
.VA where
gives the relation of the new object to
.VA poscode.
The possible values are:
.IP OBJ_CHILD
the new object is made a child of
.VA poscode .
It becomes the first child \(em that is, it is placed before any existing
children of
.VA poscode .
.IP OBJ_AFTER
the new object is placed between
.VA poscode
and the object following it, if any.
.IP OBJ_BEFORE
the new object is placed between
.VA poscode
and the object before it if any.
.LP
To insert the first object in the tree,
.VA poscode
is given as NULL, and
.VA where
as OBJ_CHILD.
.DS
remove_object(code,which)
objcode_t code;
int which;
.DE
Remove object
.VA code
or its children or descendents from the tree.
.VA which
specifies the object or objects to be removed:
.IP OBJ_SELF
The object itself is removed, if it has no children.
If it does have children, no action is taken and
.fn remove_object
returns -1.
.IP OBJ_CHILDREN
Children of
.VA code
which themselves have no children are removed.
Children which do have children are not touched.
.IP OBJ_DESCENDENTS
All descendents of
.VA code
are removed.
Thus this recursively removes all but the root of the subtree of objects
below.
.VA code .
.LP
For all removed objects, if the object's type has a
.fn free_obj
function, that function is called before the
object is destroyed.
.LP
.fn remove_object
returns 0 in all cases except when
.VA which
is OBJ_SELF and the object
.VA code
has children, when -1 is returned.
.NH 3
Traversing and manipulating the tree of objects
.LP
There is no direct access to the data structure implementing the tree of
objects \(em instead there is a function to traverse the tree:
.DS
objcode_t
get_code(code,which_rel)
objcode_t code;
int which_rel;
.DE
Returns the objcode of the relation of object
.VA code
specified by
.VA which_rel ,
which must be one of:
.IP OBJ_PARENT
the parent of
.VA code .
.IP OBJ_CHILD
the child of
.VA code .
.IP OBJ_NEXT
the object after
.VA code
in the list at this level.
.IP OBJ_PREV
the object before
.VA code .
.LP
An return value of 0 (NULL) means that the object
.VA code
was the last one in the direction specified by
.VA which_rel .
.LP
Code for visiting all the children of an object
.VA par
might look something like:
.DS
code = get_code(par,OBJ_CHILD);
for (; code != NULL; code = get_code(code,OBJ_NEXT))
	...
.DE
In fact there is a more efficient way of doing this kind of thing \(em see
.fn visit_objects
below.
.DS
visit_objects(code,rel,func,arg,p_code)
objcode_t code;
int rel, (*func)();
fval_t arg;
objcode_t p_code;
.DE
Visit each member of the list of objects starting at
.VA code ,
calling
.fn (*func)
for each object in the list.
The `next' member of the list is indicated by
.VA rel ,
which must be one of OBJ_NEXT, OBJ_PREV or OBJ_PARENT.
.LP
.fn (*func)
is called in the style
.DS
(*func)(code,arg)
objcode_t code;
fval_t arg;
.DE
where
.VA code
is the current object and
.VA arg
is the value passed to
.fn visit_objects .
.LP
If
.fn (*func)
returns non-zero at any point,
.fn visit_objects
stops at that point at returns that value to the caller.
Otherwise
.fn visit_objects
continues to the end of the list and returns zero.
.LP
If
.VA p_code
is non null,
.VA *p_code
is set to the object for which
.fn (*func)
returned non-zero, or NULL if the end of the list is reached.
.LP
There is one restriction on
.fn (*func)
\(em it may not remove the object `after' the one it is processing (that
is, the one it would normally be passed next).
It is free to remove the object it has been passed.
.DS
move_object(code,poscode,where)
objcode_t code, poscode;
int where;
.DE
Moves object
.VA code
to
.VA where
in relation to
.VA poscode
in the tree.
Any descendents move with the object.
.VA where
and
.VA poscode
have the same meanings as in
.fn new_object.
It is a fatal error to attempt to make an object a descendent of itself.
.DS
int get_num_children(parcode)
objcode_t parcode;
.DE
Returns the number of children of the object
.VA parcode .
Returns zero if
.VA parcode
has no children.
.DS
sort_children(parcode,cmp)
objcode_t parcode;
int (*cmp)();
.DE
Sorts the children of
.VA parcode ,
using the comparison function
.fn *cmp .
.LP
.fn *cmp
is called as:
.DS
(*cmp)(code1,code2)
objcode_t code1, code2;
.DE
and should return -1, 0 or 1 according as
.VA code1
should be considered less
than, equal to, or greate than,
.VA code2 .
After this call, the children of
.VA parcode
will be sorted such that for each consecutive pair of objects
.VA o1 ,
.VA o2
in the list
.VA o1
is less than or equal to
.VA o2
according to the comparison function
.fn *cmp .
.DS
change_type(objcode,newtype)
objcode_t objcode;
int newtype;
.DE
Changes the type of object
.VA objcode
to
.VA newtype ,
which must have been defined previously using
.fn define_format .
If the number of fields for objects of the new type is the same as the
number for the current type, the field list is preserved.
Otherwise the new object gets a new list of fields with undefined values.
.NH 3
Field values
.LP
.DS
set_field_value(objcode,fnum,fval)
objcode_t objcode;
int fnum;
fval_t fval;
.DE
Sets field number
.VA fnum
of object
.VA objcode
to value
.VA fval .
If
.fn get_field_value
is subsequently called for this field of this object, it will return the
value given in
.VA fval .
It is a fatal error for
.fn fnum
to be negative or too large for the number of fields for the object's type.
.DS
update_field_value(objcode,fnum,fval)
objcode_t objcode;
int fnum;
fval_t fval;
.DE
Like
.fn set_field_value ,
but does not cause an redraw of the object in the window.
This is provided for the case where the field's value has changed, but the
screen image does not need redrawing.
.DS
set_all_fields(objcode,flist,terminator)
objcode_t objcode;
fval_t *flist, terminator;
.DE
Set all the fields of object
.VA objcode
from the array of field values
.VA flist .
The end of the array is marked by an entry with the value given by
.VA terminator .
It is a fatal error for too few or too many fields to be supplied.
.DS
get_field_value(objcode,fnum)
objcode_t objcode;
int fnum;
.DE
Return the value of field
.VA fnum
of object
.VA objcode .
This returns the value previously set by
.fn set_field_value ,
.fn update_field_value
or
.fn set_all_fields .
.NH 3
Displaying the tree
.LP
.DS
void display_from(x,y)
int x,y;
.DE
Displays the tree of objects in the display area using
.VA x
and
.VA y
as offsets in pixels from the origin of the whole tree.
That is, pixel
.VA x,y ) (
relative to the whole tree will be at pixel (0,0) in the display area.
Only objects that fit completely in the display are in the y dimension are
displayed.
The application supplied select function
is called for any selected and visible objects, with the SEL_DISPLAYING bit
set in the
.VA flags
argument.
.DS
int v_scroll(npixels)
int npixels;
.DE
Scroll the display vertically
.VA npixels
pixels.
.VA npixels
may be negative, meaning scroll upwards.
Returns the number of pixels scrolled, which may be less than the number
asked for because of hitting the start or end of the information.
.LP
Any objects were displayed and as a result of the scroll would be only
partially visible are removed from the display.
Objects are only added to the display if they become completely visible in
the y dimension.
.LP
Unfortunately there is no function to do horizontal scrolling in the
current version.
.DS
int visible(code)
objcode_t code;
.DE
Returns true if object
.VA code
is visible in the display area.
.LP
Note that the object is checked for visibility in the current display \(em
if you are making changes to the tree, even with updating turned off, the
result of this function may not be constant for a given object.
If you intend to make changes to visible objects only, it is usually best
to check the visibility of all the selected objects before making any
changes.
If you do this, the
.VA se_user
field of the
.VA selst
structure may be a convenient place to record whether an object is visible
or not.
.DS
void get_position(code,p_x,p_y,p_width,p_depth)
objcode_t code;
int *p_x, *p_y, *p_width, *p_depth;
.DE
Sets
.VA *p_x ,
.VA *p_y
to the offset in pixels of the origin of object
.VA code
from the origin of the display area.
Sets
.VA *p_width ,
.VA *p_depth
to the width and depth in pixels of
.VA code .
.LP
As with
.fn visible
above, beware of using this function while you are making changes to the
tree.
.DS
objcode_t
get_object_at(x,y)
int x,y;
.DE
Returns the code of the object under the point
.VA x ,
.VA y
in the display area.
Returns NULL if 
.VA x ,
.VA y
does not lie within an object.
.DS
int get_cur_posn()
.DE
Return the current offset in pixels of the top left pixel of the display
area from the origin of the tree of objects.
In other words the last y coordinate to
.fn display_from
plus or minus any offsets caused by
.fn v_scroll .
.DS
int get_info_depth()
.DE
Returns the current depth in pixels of the entire object tree.
.NH 3
Selecting objects
.LP
.DS
void select_list()
.DE
This should be called by the application when the user moves the mouse into
the display area (the part of the window where objects are displayed).
It does direct mouse input (this will be changed in future versions).
The mouse buttons cause actions as follows:
.IP left
The current selection is cleared and the object under the cursor selected.
If the mouse is dragged with the left button down, the objects passed over
are selected.
While the mouse is being dragged all objects between the first selected
object and the object the mouse is over are selected.
If the mouse is moved back towards the originally selected object, the
objects retreated over are deselected.
.IP right
As left, but the current selection is not cleared first, and objects the
mouse is dragged over have their selection state toggled.
.IP middle
Pressing this button over an object is taken as a request to edit a field
within the object.
If there is a field editing routine for the field the user pointed at, that
routine is called.
It is expected to return when the edit is completed.
.DS
void clear_selection()
.DE
Deselects any selected objects.
.DS
select_object(code,val,which)
objcode_t code;
int val, which;
.DE
Select the relations of object
.VA code
specified by
.VA which .
If
.VA val
is non zero, the objects are added to the selection, otherwise they are removed.
The possible values of
.VA which
are:
.IP OBJ_SELF
select the object
.VA code .
.IP OBJ_CHILDREN
select the immediate children of
.VA code .
.IP OBJ_DESCENDENTS
select all the descendents of
.VA code .
.DS
struct selst {
	objcode_t se_code;
	int se_user;
	struct selst *se_next;
};

struct selst *get_selection()

struct selst *get_reverse_selection()

int get_num_selected()
.DE
.fn get_selection
returns a list of the objects currently selected.
Typically a function in the application will call this and perform some
operation on each object in the list.
The list returned is not affected by subsequent selecting or deselecting of
objects \(em it is safe for example to clear the selection before walking
through the list.
.LP
Objects in the list are ordered by level \(em that is, objects always
appear before their parents.
Other than this the order of the list is undefined.
In particular it is not ordered by time of selection by the user.
.LP
The fields of each element of the list are:
.IP se_code
The code of the selected object.
.IP se_next
The next element in the list (NULL for the last element of the list).
.IP se_user
The level of this object \(em that is, how many ancestors the object has
between it and the root of the tree of objects.
The application may set this field itself if it wishes.
The values set remain until a subsequent call of
.fn get_selection
or
.fn get_reverse_selection .
.LP
.fn get_reverse_selection
is the same as
.fn get_selection ,
except that the list returned is ordered such that objects always appear
after their parents.
.LP
.fn get_num_selected
returns the number of objects currently selected.
.DS
objcode_t get_ancestor_of_selection()
.DE
Return the nearest common ancestor of all the objects currently selected.
More precisely, imagine walking up the object tree for each object in the
selection.
At some point in the tree all the paths converge on a single object.
This is the object returned by this function.
.LP
If there is only a single object selected, the parent of that object is
returned.
If there is a selected object that is the ancestor of all the other
selected objects, the parent of that object is returned.
