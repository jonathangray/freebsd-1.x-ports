'\"
'\" Copyright 1989 Regents of the University of California
'\" Permission to use, copy, modify, and distribute this
'\" documentation for any purpose and without fee is hereby
'\" granted, provided that this notice appears in all copies.
'\" The University of California makes no representations about
'\" the suitability of this material for any purpose.  It is
'\" provided "as is" without express or implied warranty.
'\" 
'\" $Header: /a/cvs/386BSD/ports/net/tcl-dp/man/manl/distribObj.l,v 1.1 1993/09/01 00:46:32 jkh Exp $ SPRITE (Berkeley)
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
.HS DistributeObject cmds
.BS
.SH NAME
DistributeObject, UndistributeObject
.br
setf, getf 
.br
SetTrigger, SetSlotOwner
.br
.sp
  \- Tcl-DP support for replicated, distributed objects 
.BE
.LP

The distributed object commands described here are part of the
Tcl-DP (Tcl Distributed Programming) extension to Tcl/Tk.  These
Tcl/Tk commands are used to manage objects which are distributed
among several Tcl/Tk processes.  The Tcl/Tk processes must be
linked by Tcl-DP remote procedure call (RPC) connections.

An object in Tcl-DP is a procedure, just like a widget instance
of Tk is a procedure.  A procedural object must take as its first
argument a method selector.  In other words, if \fIobject\fR is
the name of a procedural object:

\fIobject\fB \fImethod\fR ?\fIargs ...\fR?

Objects can have slots.  Objects which are to be distributed
using Tcl-DP must handle two methods, \fBconfigure\fR and
\fBslot-value\fR, which access the slots of the object.

The \fBconfigure\fR method must handle three different forms of
arguments, which makes it very much like the \fBconfigure\fR
option for Tk widgets:

\fIobject\fB \fBconfigure\fR ?\fI-slot\fR?
.br
\fIobject\fB \fBconfigure\fR ?\fI-slot value ...\fR?
.br
\fIobject\fB \fBconfigure\fR

When the Tcl/Tk expression "\fIobject\fB configure\fR" is
evaluated, the return value is a list of triplets of the form
\fI{-slot default current}\fR, one triplet for each slot of the
object.  The \fIslot\fR gives the name of the slot, \fIdefault\fR
gives the default value of the slot when the object is created,
and \fIcurrent\fR gives the current value of the slot within the
object.  Furthermore, when the command "\fIobject\fB configure\fI
-slot\fR" is evaluated, the return value should be the triplet
associated with the given \fIslot\fR.  The command "\fIobject\fB
configure\fI -slot value\fR" will set the value of the \fIslot\fP
of the \fIobject\fP to the given \fIvalue\fP.

The \fBslot-value\fR method must take one argument:

\fIobject\fB \fBslot-value\fR ?\fIslot\fR?

The \fBslot-value\fR method returns the current value of a
\fIslot\fR of an \fIobject\fR.

Objects are usually defined in C for speed, with a Tcl access
interface, just like Tk widgets are defined in C but have Tcl
access interfaces.  Objects can also be defined using Tcl/Tk
scripting only, as Tcl-DP supplies several utility procedures for
creating objects with Tcl/Tk scripting only.  (See object(l).)
.PP

Each object to be distributed must also support a creator command
or \fImakeCmd\fR.  This is the command used to create an object.
It should be of the form "\fImakeCmd objName ?-slot1 value1
-slot2 value2 ...?\fR".  This command should create a procedural
object called \fIobjName\fR and initialize the slots the of new
object.
.PP

For example, suppose a point object named \fIp1\fR has two slots,
\fIx\fR and \fIy\fR, and the default value for each slot is
\fI0\fR.  Suppose the current value of \fIx\fR is \fI1\fR and the
current value of \fIy\fR is \fI2\fR.  If the \fImakeCmd\fR is
called \fIpoint\fR, then to create such an object, evaluate the
command "\fIpoint p1 -x 1 -y 2\fR".  Then the command "\fIp1
config\fR" will have the return value of \fI{{-x 0 1} {-y 0
2}}\fR, the command "\fIp1 config -x\fR" will have the return
value of \fI-x 0 1\fR, and the command "\fIp1 slot-value x\fR"
will have the return value of \fI1\fR.
.PP
Several assumptions were made in the development of the
distributed object system.  First, the graph of RPC connected
processes is assumed to be acyclic.  If this assumption is
violated, deadlock could result.  Second, each slot of an object
has an owner process.  Although the value of a slot can be set in
any process, it is most efficient to do so in the owner process.
.SH COMMANDS
.TP
\fBsetf \fIobject slot value\fR
.br
.sp
The \fBsetf\fR command is used to set the \fIvalue\fR of a
\fIslot\fR in a distributed object.  The side effect of this
command includes replicating the new \fIvalue\fR of the
\fIslot\fR to all processes where the object is distributed.
.TP
\fBgetf \fIobject slot\fR
.br
.sp
The \fBgetf\fR command is used to get the current value of a
\fIslot\fR in a distributed object.  It is the counterpart of
the \fBsetf\fR command.
.PP
.TP
\fBDistributeObject \fIobject processes makeCmd\fR
.br
.sp
\fBDistributeObject\fR arranges for \fIobject\fR to be
distributed to the given \fIprocesses\fR.  The argument
\fIobject\fR should be the name of the object to be distributed,
\fIprocesses\fR should be a list of \fIfileId\fR's (which we can
use with \fBRPC\fR) which represent connections to remote
processes, and \fImakeCmd\fR should be the creator command for
the object, as described above.
.PP
.TP
\fBUndistributeObject \fIobject processes\fR
.br
.sp
\fBUndistributeObject\fR arranges for the slots of \fIobject\fR
to not be shared anymore with remote \fIprocesses\fR.  The
\fIprocesses\fR (which should be a list of \fIfileId\fR's which
represent \fBRPC\fR connections to remote processes) supplied
should be the same as or a subset of the \fIprocesses\fR
originally passed into \fBDistributeObject\fR.  The \fIobject\fR
still exists in all processes, but its slots are no longer
shared.
.PP
.TP
\fBSetSlotOwner \fIobject slot process\fR
.br
.sp
This command is used to set the owner of a \fIslot\fR of an
\fIobject\fR to be the specified \fIprocess\fR (which is a
\fIfileId\fR that represents an \fBRPC\fR connection to a remote
process).  Although the value of a slot can be set in any
process, it is most efficient to do so in the process that owns
the slot.
.PP
.TP
\fBSetTrigger \fIobject slot cmd\fR
.br
.sp
\fBSetTrigger\fR is used to cause the Tcl command \fIcmd\fR to be
evaluated whenever a call to \fBsetf\fR causes the \fIslot\fR of
\fIobject\fR to change value.
.SH "SEE ALSO"
.PP
connect, rpc, Tcl-DP, object
.SH AUTHOR
Brian Smith (bsmith@cs.berkeley.edu)
.br
Steve Yen (syen@cs.berkeley.edu)
