.\" @(#)porting.ms	1.1 4/7/91 (UKC)
.TL
Porting the Ups Debugger
.AU
Mark Russell
.AI
University of Kent at Canterbury
\*(DY
.NH 1
Introduction
.LP
This document is a brief technical overview of the internals of the
\fIUps\fP graphical debugger, and a guide to porting it to a new
architecture.
It is assumed that the reader has used \fIups\fP and is familiar
with its user interface.
.LP
As the document is intended as a practical aid to porting ups to
a new platform, there is not much description of code that is
intended to be portable.
Most of the user interface code falls into this category.
The code that is most likely to need changing is that dealing
with the symbol table, core files, and runtime execution control.
.NH 1
Source code conventions
.LP
The following conventions are used in the ups source:
.IP \(bu
Global variable names (other than function names) start with an
upper case letter.
.IP \(bu
Local variables are user lower case only (with underscores and digits).
.IP \(bu
Enum constants and #defines use all upper case.
.IP \(bu
Typedef names use lower case and a `_t' suffix.
.IP \(bu
Many packages export opaque handles on objects.
These are typedefs to a pointer to a dummy structure, and have
the  suffix `_id_t'.
.IP \(bu
Source and header files start with a single line comment describing
the purpose of the file.
.IP \(bu
ANSI prototypes are used for all functions, with a macro (`PROTO')
to hide the prototypes when compiling with an non-ANSI C compiler.
The definitions of functions use old style C.
.IP \(bu
The source is written to compile under gcc with all warnings turned
on, and with the -Wwrite_string flag, which makes string literals
have type `const char *'.
All string pointers that will not be written through are declared
`const char *'.
.IP \(bu
Prototypes for static functions appear at the top of the source file.
Prototypes for external functions appear in a header file, which
usually has the same name as the source file except for the .h
suffix.
.IP \(bu
Places where things should really be done differently are marked
with a comment starting with `BUG:'.
Code which is particularly machine or compiler dependent is marked
with a comment starting with `FRAGILE CODE:'.
.NH 1
Major modules
.LP
\fIUps\fP is built from a number of modules and supporting libraries.
Some are separated into a separate library directory; others are
implemented in a set of source files in the ups source directory,
usually all starting with the same prefix.
.LP
The libraries are:
.IP libmtrutil
Utility functions: things like a fast memory pool allocation package
(alloc_pool.c), random access to lines in a text file (so.c) and
generalised error message handling (mesgf.c).
This is where header files like gendefs.h (containing project wide
#defines like TRUE and FALSE) live.
.IP libx11wn
A window system portability library that runs on top of X11 or SunView.
There is no X code in ups; it is written entirely using Wn.
Wn's interface is at about the same level as Xlib, but with much
simplified functionality.
See the TeX document in doc/wn.tex.
.IP libobj
The low level implementation of the ups display area.
The Obj library handles the layout and display of objects of different
types, with application provided callbacks to handle rendering and
input.
It is conceptually (very) roughly at the same level as the X toolkit.
.IP libarg
Shell-like command line parsing (mostly globbing and i/o redirection).
This is used to parse the target command line in the ups display area.
.IP libMen3
Generalised menu handling.
This library (designed and implemented by John Bovey, University of Kent)
deals with the rendering and event handling of arbirtary menus.
The menu description files are created by an interactive menu editor
(not included in this source distribution).
.LP
The modules are:
.IP "the symbol table"
This module does a prescan of the target's symbol table on startup.
It builds an internal list of functions, global variables and source
files.
There are routines to load more complete symbol table information
for particular functions or source files on demand.
The source and header files for this package are in files called
st_*.c.
.IP "the user interface"
This is a fairly loosely defined module concerned with the user interface.
The module prefix is `ui_'.
There is lots of user interface code outside this module (e.g. the code
that implements the mouse hole and thumb bars).
The code here does things like the division of the window into
subregions, the main event loop and the distribution of events and
part of the code that handle the source window.
.IP "machine code disassembly"
This is not so much a module as a collection of disassemblers for
various instruction sets (currently the VAX, 680X0, MIPS, SPARC and Clipper).
Ups does not need to disassemble the code, but it can do single
stepping more efficiently if it can.
The module prefix is `as_'.
.IP "object handling"
The various objects on the display area are created with the object
library (see the document `The Obj library' for a description of
this library).
The routines in this module (prefix `obj_') sit on top of the object
library.
They implement the rendering of objects in the display area, and
the editing of fields in the objects.
.IP "the C interpreter"
This module implements the C interpreter, which is used when the user
edits in C code at a breakpoint.
The prefix is `ci_'.
All functions exported from this module have prototypes in the header
file `ci.h'.
.IP "variables"
This module handles the display of variables and expressions in the
stack trace and under source files, the editing of subscripts and the commands
in the variables menu.
It also handles the constructions of C declarations from the internal
ups symbol table information.
.LP
There are a number of other important source files:
.IP proc.c
Low level control of target processes.
.IP core.c
Interpretation of core files for post-mortem debugging.
.IP stack.c
Code to build a representation of the target's execution stack (from
a process or core file).
.IP "trun_j.c, trun_ss.c"
These are two alternative implementations of target execution control.
Trun_j.c requires knowledge of the machine instruction set, and puts
breakpoints at jumps to implement the `next' and `step' commands.
.IP
Trun_ss.c doesn't need to know the instruction set \(em it
implements `next' and `step' by stepping the target a single machine
instruction at a time.
This can be slow for source lines that are translated to a lot of
machine instructions (it is currently only used for the Sun 386i).
.IP src.c
Low level handling of source files.
Handles the addition of internal lines (used with breakpoints).
.IP textwin.c
Low level handling of the display of text files in a window.
These routines are used by src.c and output.c to display text in
their respective windows.
.IP libmtrutil/so.c
Random access to text files.
The routines here read a file to build a table of line offsets, and
pull in wanted lines on demand.
Used to read and display source lines.
.IP libmtrutil/strcache.c
Random access to strings in a file, with some buffering.
Used by the symbol table routines to read the symbol table strings
section, and by strcache.c to read text file lines.
.NH 1
Functional overview
.LP
When ups is started the following things happen (assuming no errors):
.IP
\fIMain\fP() in ups.c parses the command line arguments and opens the
specified text file (\fIopen_textfile\fP() in st_read.c).
It calls \fIget_and_install_symtabs\fP() in st_stab.c to do a prescan
of the symbol table of the text file.
.IP
If a core file is specified it is opened and checked against the
text file (\fIopen_corefile\fP() in core.c).
\fIMain\fP() then passes control to \fIups\fP() in ups.c, giving it arguments
to specify the text and core files.
.IP
\fIUps\fP() in ups.c opens the main window (by calling the Wn function
\fIwn_open_stdwin\fP()).
It then calls \fIdivide_window_into_regions\fP() in ui_layout.c to set up
all the subregions of the display.
The lines that divide the display area are drawn by this function,
and the callback functions that handle input and output in the various
regions are established.
At this point the display is drawn but most of the regions are empty.
.IP
\fIUps\fP() then calls \fIinitialise_display_area\fP() in obj_buildf.c, which adds
the various objects in the display area (`Source files', `Functions'
etc).
This consists mostly of calls the object library.
.IP
Finally, \fIups\fP() calls \fIre_event_loop\fP() in reg.c.
This is the main input loop of ups.
It reads events from the Wn library, works out which region of the
display they are intended for and invokes the callback function
for the region.
When the input loop terminates, ups exits.
.NH 1
The symbol table
.LP
A large body of code in ups is concerned with extracting symbol
table information from the target.
We cover this in some detail here as it is quite likely to need
changes when porting ups to a new machine.
If you are lucky the machine will use the standard 4.3BSD symbol
table format and no changes will be necessary.
If you are unlucky the machine will define a weird and wonderful
format all its own, and you will be into major hacking of the
symbol table code.
.NH 2
Information needed from the symbol table
.LP
The information ups needs is:
.IP \(bu
names and addresses of functions.
.IP \(bu
names and addresses of global and local variables.
.IP \(bu
type information for variables, including the bounds and number
of dimensions of arrays.
.IP \(bu
scope information for functions \(em where blocks start and end,
and which blocks local variables are declared in.
.IP \(bu
for each structure or union, its tag and a list of the names and offsets
of its members.
.IP \(bu
for each enumerated type, its tag and list of the names and values
of the constants of that type.
.IP \(bu
for each source file, its name and a map of source file line numbers
to target addresses.
.NH 2
External symbol table formats
.LP
Ups currently knows about the 4.3BSD symbol table format,
which is documented in \fIdbx\fP(5) on 4.3BSD systems.
This format is used with 4.3BSD and Ultrix on the VAX.
.LP
Under SunOS the symbol table has been modified in an attempt to
reduce symbol table size.
In the standard format type definitions can refer to other type
definitions (saying things like "type 13 is a pointer to type 10").
In the SunOS version, you can say "type 13 in file 17 is a pointer to
type 10 in file 7").
Header files are numbered, and when possible only one copy of the type
information from a header file is included.
.LP
This scheme can substantially reduce the size of a symbol table,
especially when an object file is made up of many small source files
each of which #includes a lot of header files (e.g. Xlib).
It does mean that ups has to do a lot of bookkeeping to correctly
decode the symbol table references.
.LP
The symbol table format is more or less the same on the Sun 386i,
except that it has been wrapped up as a COFF (Crippled Object
File Format) symbol table.
As usual with COFF, it just gets in the way.
.NH 3
Shared libraries
.LP
The symbol table code is complicated further by SunOS shared libraries.
Each shared library has its own symbol table, so ups has to cope with
multiple symbol tables for an object file.
Routines that do things like map an name to an address have to loop
over all symbol tables of a shared library object file.
For simplicity, non shared object files (and object files for
other platforms) are treated as just happening to have only one
symbol table \(em the various symbol table loops iterate once
only in this case.
.LP
One problem with shared libraries is that the addresses of global
variables and functions are relative to the address the shared
library gets mapped to at run time.
This means we have to have code that walks over a symbol table
adjusting all the global addresses by a given delta (see
\fIchange_text_addr_offset\fP() in st_stab.c).
.LP
We go through some contortions to allow the user to set breakpoints
in shared library routines before the target has run.
This avoids dbx's irritating behaviour of saying `unknown function
printf' if you try to put a breakpoint in printf before the
target has been run.
.NH 2
The internal symbol table
.LP
The internal symbol table format is a tree of various types of
structures, defining the different symbol table entities
(source files, functions, variables, types etc).
The root of this is the symtab_t structure, defined in 
st_priv.h as:
.DS
typedef struct symtabst {
	const char *st_name;		/* Name of the a.out file */
	symtab_type_t st_type;		/* Type (STT_MAIN or STT_SHLIB) */
	int st_dynamic;			/* TRUE if dynamically linked */
	target_info_t *st_target_info;	/* Per target information */

	alloc_id_t st_alloc_id;		/* Id for alloc pool of this symtab */

	long st_text_addr_offset;	/* Offset in process of start of text */

	symio_id_t st_symio_id;		/* Symbol input from a.out file stuff */
	fil_t *st_sfiles;		/* List of source files */
	cblist_id_t st_cblist_id;	/* List of FORTRAN common blocks */
	functab_id_t st_functab_id;	/* Addr --> func mapping table */
	addrlist_id_t st_addrlist_id;	/* List of addresses of globals */

	struct symtabst *st_next;	/* Next symbol table */
} symtab_t;
.DE
.LP
When using SunOS shared libraries there is a chain of these linked
by the st_next pointer.
Otherwise st_next is NULL.
This structure is private to the symbol table routines.
Other routines refer to it by the opaque handle type symtab_id_t
defined in symtab.h (the public header file for the symbol table
routines).
.LP
The \fIMain_st\fP global variable points at the symbol table list for
the target.
As everything else in the internal table hangs off the symtab_t structure
it would not be hard to make ups handle multiple processes.
The various functions that refer to \fIMain_st\fP would have to be changed
to take an explicit \fIsymtab_id\fP argument.
.LP
The internal symbol hierarchy is defined in the public symbol table
header file symtab.h.
It looks like this:
.IP
Each symtab_t structure gives the symbol table for a single object file
or shared library.
Off it hangs a list of source files making up the object file,
and an opaque handle on a table mapping functions addresses to
names for all the functions in the object file (see st_fmap.c).
It also has an opaque handle on a list mapping global variable
addresses to names and types.
.IP
Each source file structure (fil_t, defined in symtab.h) has
(among other things) a list of functions defined in that source file,
an opaque handle on a table of line number offsets into the source file
set up and used by code in so.c, and a list of global variables
defined in the source file.
.IP
Each function (func_t, defined in symtab.h) has a pointer to the source
file it was defined in, the address in the target of the function,
and pointers to line number and local variable information.
.IP
The line number and local variable information is only loaded if
necessary (usually as a result of the user adding a variable to a
function enrry appearing in the stack trace).
.IP
The local variable information for a function consists of a description
of the block structure (as C allows variables to be declared at the start
of any block).
The information for a block (block_t, defined in symtab.h) consists
of the start end and line numbers for the block, a pointer to the
list of variables declared in the block, a pointer to the next
block at the same level as this one, and a pointer to a list of
blocks declared in this one.
.IP
The structure describing a variable (var_t, defined in symtab.h) gives
the name, type, storage class, and address of the variable.
The address is either the absolute address, the offset from the
frame pointer, the offset from the start of a structure or union 
or a register number, depending on the class of the variable
(see \fIget_dv_addr\fP() in va_menu.c).
See below for the encoding of types.
.NH 3
Internal representation of types
.LP
C types are represented in ups as a linked list, with an element in
the list (of type type_t) for each derivation (`pointer to', `array of',
or `function returning').
Each element in the list has a code (ty_typecode) which is either one
of the above derivations, or a basic type.
A basic type is either one of the C standard types, or an
aggregate (`struct', `union' or `enum'). 
A type_t has a union which contains pointers to more information
about the type.
The ty_typecode member determines which element of the union should
be used.
.NH 1
The C interpreter
.LP
This is fairly separate from the rest of ups.
The interface to the interpreter is defined in the public header file
ci.h.
For testing purposes, there is a driver for the C interpreter (cx.c)
which allows it to be run on C source files, independently of ups.
.LP
The interpreter code uses the ups symbol table format internally,
and has various generic hooks in it to allow it to work with ups.
.LP
The most important of these is the distinction between variables
declared in interpreted code and those already existing in the
target.
When ups calls the interpreter to handle a fragment of code attached
to a breakpoint it passes in a routine to handle references to
variables in the target.
.LP
This document does not describe the interpreter as the code should
not need changing \(em it should be reasonably portable. 
.NH 1
Target execution control
.LP
This section describes how ups controls the execution of the target
process, to implement breakpoints and the `next' and `step' commands.
.LP
Ups uses the \fIptrace(2)\fP system call to control the target on all
the existing platforms.
This has been reasonably portable between SunOS on the 386i, Sun 3 and
SPARC, and Ultrix on the VAX and DECstation 3100.
All use of ptrace is in the file proc.c, which implements low level
access to the target.
This is the only file that should be affected if, say, you want to
put ups on a machine with a /proc filesystem instead of ptrace.
See proc.h for the higher level abstraction of target control
implemented by proc.c.
.NH 2
Breakpoints
.LP
Proc.c implements breakpoints by using ptrace to overwrite the text
of the target at the point of the breakpoint with a trap opcode of some
sort.
When the target hits the trap opcode the kernel maps this to a
SIGTRAP.
The routine that waits for the target to hit a breakpoint
(\fIwait_for_target\fP()) recognises SIGTRAP as a special case.
Machines vary as to the program counter value reported when
a breakpoint trap is hit \(em some report the address of the
trap, some the address after it.
There is some ifdefed code in \fIupdate_regs\fP() to hide this
wrinkle from higher level code.
.LP
If you are porting to a new architecture you will have to find out
what the trap opcode is.
You may find the machine supports setting breakpoints simply by
specifying the address, in which case you'll have to modify the
breakpoint handling code.
The abstraction provided by proc.c is not very good here \(em it
exports the idea of fiddling with the text of the target as a
way of setting breakpoints to higher routines.
This needs fixing.
.NH 2
Registers
.LP
The model of registers exported by proc.c is that there are a number
of special purpose registers and some general purpose registers,
which are all the same width.
The routine \fIproc_getreg\fP() takes a register number which is either
positive (indicating a general register) or a #defined negative value.
Negative values refer to special purpose registers as follows:
.IP REG_PC
the program counter
.IP REG_FP
the frame pointer
.IP REG_AP
the argument pointer (see below)
.IP REG_SP
the stack pointer
.LP
Higher levels of software in ups assume that local variable addresses
are offsets from the frame pointer, and that addresses of arguments to
routines are relative to the argument pointer.
On most machines the argument pointer is the same as the frame pointer;
REG_AP is provided for the benefit of the VAX, where they are \fInot\fP the
same register.
.LP
If this does not match your machine's model, you have some serious
hacking to do.
.LP
Look at \fIproc_getreg\fP() and the routines it calls to get an idea of
how to implement its functionality on a new machine.
.NH 2
Calling target functions
.LP
Ups allows interpreted code at breakpoints to call functions compiled
into the target.
The low level implementations of this is in \fIproc_call_func\fP().
This routine simulates a function call in the target, by pushing
the arguments to be passed onto the target's stack (writing to the
stack with ptrace), updating the target's stack pointer register,
and using ptrace to jump to the start of the called routine.
It then reads the return value from the return register.
.LP
Needless to say, this stuff is totally machine and compiler dependent,
and will need rewriting for a new architecture.
It may not even be possible in some architectures, it which case
you'll have to make it give an error message and return.
.NH 2
Reading and writing target data
.LP
This is implemented with \fIproc_read_data\fP() and \fIproc_write_data\fP().
They both take an address, a buffer to be read or written and
a byte count.
.LP
Under SunOS these calls map directly to ptrace routines which have
a similar interface.
On other machines ptrace will only read or write a word at a time,
and these calls have to be implemented as loops (horribly inefficient
for large requests, as it involves a system call for each word).
.NH 1
The stack trace
.LP
Extracting the stack trace from the target involves some tricky
machine and and compiler dependent code.
.LP
The basic idea is fairly simple, at least for the current platforms.
The stack consists of a linked list of frames.
The frame pointer register points at the innermost frame, and each
frame has a saved frame pointer that points to the next frame out.
The list ends when a NULL frame pointer is reached.
Also stored with each frame is a saved program counter value.
.LP
There are several things that complicate this simple picture.
The worst offender is signal handler routines.
The code to detect and decode these in the stack trace is fragile,
machine dependent and depends on undocumented features of how the
C compiler and kernel works.
See \fIbuild_stack_trace\fP() in stack.c for the gory details.
.LP
Another complication is that some optimised leaf routines don't bother
to set up a frame pointer.
These routines never call other routines, but things get complicated
if a signal handler gets invoked while we are in such a routine.
.LP
This code may be hard to port to a new machine if it lacks the idea
of a frame pointer.
Usually though, compilers on platforms like this either have an
option to use a frame pointer, or the machine has a
`virtual' frame pointer.
.NH 1
Core files
.LP
The code dealing with core files (for post-mortem debugging) is
machine dependent but fairly simple.
The function \fIopen_corefile\fP() opens the file whose
name is given as an argument, and if it can checks that the file
is a core dumped from the target.
Assuming it is, it sets the fields of the \fICoredesc\fP structure.
.LP
The core file access routines are then more or less portable.
They allow reading of data from a given address (\fIcore_dread\fP())
and getting a register value from the core file (\fIcore_getreg\fP()).
.LP
To port this to a new platform, you will have to discover the
structure of core files.
You'll need address to file mappings for the data and stack areas,
the location of the saved registers and preferably the command name
so you can check it against the text file name.
.NH 1
Porting strategy
.NH 2
Resources needed.
.LP
The resources you will need for this are:
.IP \(bu
the \fIups\fP source code, and the source for it's supporting
libraries (libmtrutil, libx11wn, libobj, libarg and libMen3).
.IP \(bu
Documentation of the layout of executable files.
.IP \(bu
Documentation on the symbol table format used on your platform.
.IP \(bu
Some knowledge of the platform's CPU architecture (mainly how
the stack works, and what registers it has).
You don't need to know the details of the instruction set,
but you will need to know how to cause a breakpoint trap.
.IP \(bu
Knowledge to the C compiler function calling protocol.
This can range from simple (the VAX) to complex (the DS3100).
The bad news is that the various RISC CPUs tend to use more
complex calling protocols to increase speed (e.g. not using 
a frame pointer, passing arguments in registers, etc).
.LP
If you are lucky, all this stuff will be documented.
If you are unlucky, you'll have to figure it out by poring
over assembler output from the compilers and writing
programs to dump symbol table information in ASCII.
.NH 2
Strategy
.LP
The first step in porting \fIups\fP to a new platform is simply
to say
.DS
make -k
.DE
in the \fIups\fP source directory.
Lots of the code (particularly the user interface code) is machine
independent and will compile without change.
At the end of the make do a `make -n' to see which files have not
compiled.
Almost certainly these will include \fIproc.c\fP, \fIcore.c\fP,
and \fIstack.c\fP.
Some of the symbol table code will probably not compile \(em how
much depends on how strange the symbol table format is on the new
machine.
.LP
Before you start hacking the code, you'll probably want to define
a preprocessor symbol for the new platform to ifdef the code.
Look in mtrutil/ifdefs.h for the scheme used in \fIups\fP.
Based in the predefined macros for your machine, define suitable
\fIOS_XXX\fP and \fIARCH_XXX\fP macros.
.LP
The first step is usually to get the symbol table code working.
This can be anything from a few hours to several weeks of work,
depending on the symbol table format your machine uses.
During this stage it's usually worth putting in dummy code to
make the rest of \fIups\fP compile (e.g. defining the breakpoint
opcode as zero).
.LP
The next step is usually to make \fIups\fP work with core files.
This lets you concentrate on decoding the stack trace
(\fIbuild_stack_trace\fP() in stack.c) without have to worry
about target control.
Initially you probably want to ignore nasty bits like signals
and routines without frame pointers.
.LP
Once you have \fIups\fP successfully displaying the stack trace
(i.e. giving the right list of functions), you can work on the
symbol table dependent things like getting the line number mapping
working OK, and displaying local variables.
Start with simple variables, and then try arrays and structures.
One problem area is register variables that have been saved on the
stack \(em you'll have to find out what your compiler does and
write the appropriate code.
.LP
The final step is target execution control.
If your system has the \fIptrace\fP() system call in a reasonably
similar form to the existing ports this may be fairly easy.
You'll need to find a way of setting a breakpoint.
Look for TRAP or BPT type opcodes in the instruction sets, and
at any comments on SIGTRAP in signal.h.
Failing that, find out what existing debuggers on the architecture
do (system call tracing programs are useful for this if you have
them).
.LP
Finally an appeal: if you do a port to a new architecture then please
send the changes back to me (mtr@ukc.ac.uk) so I can fold them into
the standard release.
This way everybody benefits.
You will get due credit in the standard documentation and source code.
