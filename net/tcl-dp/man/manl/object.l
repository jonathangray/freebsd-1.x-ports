'\"
'\" Copyright 1989 Regents of the University of California
'\" Permission to use, copy, modify, and distribute this
'\" documentation for any purpose and without fee is hereby
'\" granted, provided that this notice appears in all copies.
'\" The University of California makes no representations about
'\" the suitability of this material for any purpose.  It is
'\" provided "as is" without express or implied warranty.
'\" 
'\" $Header: /a/cvs/386BSD/ports/net/tcl-dp/man/manl/object.l,v 1.1 1993/09/01 00:46:32 jkh Exp $ SPRITE (Berkeley)
'
.\" The definitions below are for supplemental macros used in Sprite
.\" manual entries.
.\"
.\" .HS name section [date [version]]
.\"	Replacement for .TH in other man pages.  See below for valid
.\"	section names.
.\"
.\" .AP type name in/out [indent]
.\"	Start paragraph describing an argument to a library procedure.
.\"	type is type of argument (int, etc.), in/out is either "in", "out",
.\"	or "in/out" to describe whether procedure reads or modifies arg,
.\"	and indent is equivalent to second arg of .IP (shouldn't ever be
.\"	needed;  use .AS below instead)
.\"
.\" .AS [type [name]]
.\"	Give maximum sizes of arguments for setting tab stops.  Type and
.\"	name are examples of largest possible arguments that will be passed
.\"	to .AP later.  If args are omitted, default tab stops are used.
.\"
.\" .BS
.\"	Start box enclosure.  From here until next .BE, everything will be
.\"	enclosed in one large box.
.\"
.\" .BE
.\"	End of box enclosure.
.\"
.\" .VS
.\"	Begin vertical sidebar, for use in marking newly-changed parts
.\"	of man pages.
.\"
.\" .VE
.\"	End of vertical sidebar.
.\"
.\" .DS
.\"	Begin an indented unfilled display.
.\"
.\" .DE
.\"	End of indented unfilled display.
.\"
'	# Heading for Sprite man pages
.de HS
.if '\\$2'cmds'       .TH \\$1 1 \\$3 \\$4
.if '\\$2'lib'        .TH \\$1 3 \\$3 \\$4
.if '\\$2'tcl'        .TH \\$1 3 \\$3 \\$4
.if '\\$2'tk'         .TH \\$1 3 \\$3 \\$4
.if t .wh -1.3i ^B
.nr ^l \\n(.l
.ad b
..
'	# Start an argument description
.de AP
.ie !"\\$4"" .TP \\$4
.el \{\
.   ie !"\\$2"" .TP \\n()Cu
.   el          .TP 15
.\}
.ie !"\\$3"" \{\
.ta \\n()Au \\n()Bu
\&\\$1	\\fI\\$2\\fP	(\\$3)
.\".b
.\}
.el \{\
.br
.ie !"\\$2"" \{\
\&\\$1	\\fI\\$2\\fP
.\}
.el \{\
\&\\fI\\$1\\fP
.\}
.\}
..
'	# define tabbing values for .AP
.de AS
.nr )A 10n
.if !"\\$1"" .nr )A \\w'\\$1'u+3n
.nr )B \\n()Au+15n
.\"
.if !"\\$2"" .nr )B \\w'\\$2'u+\\n()Au+3n
.nr )C \\n()Bu+\\w'(in/out)'u+2n
..
'	# BS - start boxed text
'	# ^y = starting y location
'	# ^b = 1
.de BS
.br
.mk ^y
.nr ^b 1u
.if n .nf
.if n .ti 0
.if n \l'\\n(.lu\(ul'
.if n .fi
..
'	# BE - end boxed text (draw box now)
.de BE
.nf
.ti 0
.mk ^t
.ie n \l'\\n(^lu\(ul'
.el \{\
.\"	Draw four-sided box normally, but don't draw top of
.\"	box if the box started on an earlier page.
.ie !\\n(^b-1 \{\
\h'-1.5n'\L'|\\n(^yu-1v'\l'\\n(^lu+3n\(ul'\L'\\n(^tu+1v-\\n(^yu'\l'|0u-1.5n\(ul'
.\}
.el \}\
\h'-1.5n'\L'|\\n(^yu-1v'\h'\\n(^lu+3n'\L'\\n(^tu+1v-\\n(^yu'\l'|0u-1.5n\(ul'
.\}
.\}
.fi
.br
.nr ^b 0
..
'	# VS - start vertical sidebar
'	# ^Y = starting y location
'	# ^v = 1 (for troff;  for nroff this doesn't matter)
.de VS
.mk ^Y
.ie n 'mc \s12\(br\s0
.el .nr ^v 1u
..
'	# VE - end of vertical sidebar
.de VE
.ie n 'mc
.el \{\
.ev 2
.nf
.ti 0
.mk ^t
\h'|\\n(^lu+3n'\L'|\\n(^Yu-1v\(bv'\v'\\n(^tu+1v-\\n(^Yu'\h'-|\\n(^lu+3n'
.sp -1
.fi
.ev
.\}
.nr ^v 0
..
'	# Special macro to handle page bottom:  finish off current
'	# box/sidebar if in box/sidebar mode, then invoked standard
'	# page bottom macro.
.de ^B
.ev 2
'ti 0
'nf
.mk ^t
.if \\n(^b \{\
.\"	Draw three-sided box if this is the box's first page,
.\"	draw two sides but no top otherwise.
.ie !\\n(^b-1 \h'-1.5n'\L'|\\n(^yu-1v'\l'\\n(^lu+3n\(ul'\L'\\n(^tu+1v-\\n(^yu'\h'|0u'\c
.el \h'-1.5n'\L'|\\n(^yu-1v'\h'\\n(^lu+3n'\L'\\n(^tu+1v-\\n(^yu'\h'|0u'\c
.\}
.if \\n(^v \{\
.nr ^x \\n(^tu+1v-\\n(^Yu
\kx\h'-\\nxu'\h'|\\n(^lu+3n'\ky\L'-\\n(^xu'\v'\\n(^xu'\h'|0u'\c
.\}
.bp
'fi
.ev
.if \\n(^b \{\
.mk ^y
.nr ^b 2
.\}
.if \\n(^v \{\
.mk ^Y
.\}
..
'	# DS - begin display
.de DS
.RS
.nf
.sp
..
'	# DE - end display
.de DE
.fi
.RE
.sp .5
..
.de UL
\\$1\l'|0\(ul'\\$2
..
.HS object tcl
.BS
.SH NAME
.LP 
objectCreateProc, objectExists, objectFree, objectConfigure
.br
objectSlot, objectSlotSet, objectSlotAppend, objectSlots 
.br
  \- object manipulation using Tcl scripting
.SH SYNOPSIS
\fBobjectCreateProc\fR \fIclass\fR \fIobject\fR
.br
.sp
\fBobjectExists\fR \fIobject\fR
.br
\fBobjectSlots\fR \fIobject\fR
.br
\fBobjectFree\fR \fIobject\fR
.br
.sp
\fBobjectSlot\fR \fIobject\fR \fIslot\fR
.br
\fBobjectSlotSet\fR \fIobject\fR \fIslot\fR \fIvalue\fR
.br
\fBobjectSlotAppend\fR \fIobject\fR \fIslot\fR \fIvalue\fR
.br
.sp
\fBobjectConfigure\fR \fIclass\fR \fIobject\fR \fIoptions\fR
.BE
.SH DESCRIPTION
.LP

A procedural object is a Tcl procedure that represents an object.
The utility commands described in this man page allow one to
easily implement procedural objects using Tcl scripting only.

A procedural object is usually implemented in C (e.g., as a C
struct) for speed but has a Tcl access interface.  For example,
Tk implements its widget classes in C but provides Tcl interfaces
to access those widgets.  Tk uses a procedure to represent each
widget instance.

The first argument of one of these procedural objects is a method
selector.  In other words, if \fIobject\fR is the name of a
procedural object:

\fIobject\fB \fImethod\fR ?\fIargs ...\fR?

If you use the utility procedures described in this man page to
implement a procedural object, that object will have a class and
may have slots.  The class of the object determines what methods
the object will respond to.  No class inheritance is supported.
.SH COMMANDS 
.TP 
\fBobjectCreateProc \fIclass\fR \fIobject\fR

This command creates a new Tcl procedure to represent a new \fIobject\fR.
The new procedure will have the name \fIobject\fR and have the given
\fIclass\fR.
.TP
\fBobjectExists \fIobject\fR

This command returns 1 if a procedural object named \fIobject\fR
exists in the interpreter and returns 0 otherwise.
.TP
\fBobjectFree \fIobject\fR

This command frees up the resources associated with the
\fIobject\fR, which should have been created with
\fBobjectCreateProc\fR.  After using \fBobjectFree\fR on an
\fIobject\fR, no more references to that \fIobject\fR or its slots
should be made.
.TP
\fBobjectSlots \fIobject\fR

This command returns a list of the slots of the procedural
\fIobject\fR, which should have been created with
\fBobjectCreateProc\fR.
.TP
\fBobjectSlot \fIobject\fR \fIslot\fR

This command returns the value of a \fIslot\fR of the procedural
\fIobject\fR, which should have been created with
\fBobjectCreateProc\fR.
.TP
\fBobjectSlotSet \fIobject\fR \fIslot\fR \fIvalue\fR

This command sets the \fIvalue\fR of a \fIslot\fR of the
\fIobject\fR, which should have been created with
\fBobjectCreateProc\fR.  If \fIslot\fR is not yet a slot of the
\fIobject\fR, this command will make \fIslot\fR a slot of the
object.  Thus, slots can be dynamically added to objects created
using \fBobjectCreateProc\fR.  A \fIslot\fR may not be an array.
.TP
\fBobjectSlotAppend \fIobject\fR \fIslot\fR \fIvalue\fR

This command uses \fBlappend\fR to append the given \fIvalue\fR
to the current value of the \fIslot\fR of the \fIobject\fR, which
should have been created with \fBobjectCreateProc\fR.  If
\fIslot\fR is not yet a slot of the \fIobject\fR, this command
will make \fIslot\fR a slot of the object.  Thus, slots can be
dynamically added to objects created using
\fBobjectCreateProc\fR.
.TP
\fBobjectConfigure \fIclass\fR \fIobject\fR ?\fIoptions\fR?

This command provides a Tk "configure" style of slot access for
the procedural \fIobject\fR, which should have been created using
\fBobjectCreateProc\fR.  (See example usage below.)  The argument
\fIclass\fR should be the class of the \fIobject\fR.  The
optional \fIoptions\fR argument can be used in the following
three formats:

\fBobjectConfigure \fIclass\fR \fIobject\fR ?\fI-slot\fR?
.br
\fBobjectConfigure \fIclass\fR \fIobject\fR ?\fI-slot val -slot val ...\fR?
.br
\fBobjectConfigure \fIclass\fR \fIobject\fR
.br

The first format of \fBobjectConfigure\fR retrieves the
configuration entry for a given \fIslot\fR.  A configuration
entry for a \fIslot\fR is a triplet of the format of \fI{-slot
default value}\fR, where \fI-slot\fR is the slot name prepended
with a hyphen, \fIdefault\fR is the default value for the
\fIslot\fR, and \fIvalue\fR is the current value of the
\fIslot\fR.  (Note that this configuration entry format for slots
is different than the configuration entry format of Tk widget
options.)

The second format of \fBobjectConfigure\fR sets the current
values of several \fIslot\fRs of an \fIobject\fR.  

The third format of \fBobjectConfigure\fR retrieves the
configuration entries of all the slots of the \fIobject\fR.

Note that to use \fBobjectConfigure\fR, you must prepend a hyphen
('-') to each slot name.  (e.g., "slot" becomes "-slot".)  This
was designed for pseudo-compatability with the Tk style of widget
option configuration, since widget options in Tk are all prefixed
with a '-'.  Also, any slot names, \fIslot\fR, should be fully
specified and not abbreviated.
.SH "CLASSES AND METHODS"
.LP

The command \fBobjectCreateProc\fR creates a new Tcl procedure whose
name is \fIobject\fR to represent a new procedural object.  
You may invoke various methods on the procedural \fIobject\fR,
using the following general form:
.DS C
\fIobject method \fR?\fIarg0 arg1 ...\fR?
.DE

The \fImethod\fR argument and the \fIclass\fR of the \fIobject\fR
determine the method invoked.
.LP
Methods for classes are specified using the following form:
.DS C
proc \fIclass\fR.\fImethod\fR {\fIself arg0 ... argN\fR} {
   ...method body...
}
.DE

The first argument \fIself\fR is required in the method
specification.  The \fIself\fR argument is set to the name of the
\fIobject\fR that the \fImethod\fR will get called upon.
.SH EXAMPLE
.LP
The following is a simple example of using the above commands to
create a procedural object using Tcl scripting only:
.DS C
# Define point creator.
#
proc makePoint {aPoint} {
   objectCreateProc point $aPoint;
   objectSlotSet $aPoint x 0;
   objectSlotSet $aPoint y 0;
   return $aPoint;
}

# Define methods of point class.
#
proc point.configure {aPoint args} {
   return [eval objectConfigure point $aPoint $args];
}

proc point.slot-value {aPoint slot} {
  return [objectSlot $aPoint $slot];
}

proc point.draw {aPoint} {
   set x [objectSlot $aPoint x];
   set y [objectSlot $aPoint y];
   dot $x $y;
}

# Instatiate a point and test it out.
#
makePoint p;
p configure -x 10 -y 20;
p configure -x;
p configure;
p slot-value x;
p draw;
objectExists p;
objectFree p;
.DE
.SH KEYWORDS
object, method, class, slots, associative arrays, configure, options,
prototyping
.SH AUTHOR
Brian Smith (bsmith@cs.berkeley.edu)
.br
Steve Yen (syen@cs.berkeley.edu)
