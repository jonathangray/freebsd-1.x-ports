'\"
'\" Copyright 1989 Regents of the University of California
'\" Permission to use, copy, modify, and distribute this
'\" documentation for any purpose and without fee is hereby
'\" granted, provided that this notice appears in all copies.
'\" The University of California makes no representations about
'\" the suitability of this material for any purpose.  It is
'\" provided "as is" without express or implied warranty.
'\" 
'\" $Header: /a/cvs/386BSD/ports/net/tcl-dp/man/manl/Tcl-DP.l,v 1.1 1993/09/01 00:46:32 jkh Exp $ SPRITE (Berkeley)
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
.HS Tcl-DP cmds
.BS
.SH NAME
.LP 
Tcl-DP - distributed programming extension to Tcl/Tk
.BE
.SH DESCRIPTION
.LP

Tcl-DP is a freely distributable extension to Tcl/Tk and stands
for Tcl Distributed Programming.  Tcl-DP adds TCP and IP
connection management, remote procedure call (RPC), and
distributed object support to Tcl/Tk.  Tcl stands for Tool
Command Language, a freely distributable, embeddable scripting
language.  Tk stands for Toolkit, a freely distributable X
windows widget set extension to Tcl.

This man page provides an overview of the Tcl-DP programmer
interface.  The Tcl/Tk commands provided by Tcl-DP can be
separated into three levels:

.SH CONNECTION MANAGEMENT COMMANDS
.LP

Tcl-DP adds to Tcl/Tk several commands which manage basic TCP and
IP network connections.  Among these connection management
commands are \fBaddress\fR, \fBaccept\fR, \fBconnect\fR,
\fBshutdown\fR, \fBsend\fR, \fBreceive\fR, \fBsendTo\fR,
\fBreceiveFrom\fR, and \fBfilehandler\fR.  These commands provide
basically the same level of service as the UNIX C \fBsocket\fR(2)
related system calls.

These connection management commands return and take as arguments
identifiers called \fIfileId\fR's.  (A \fIfileId\fR represents a
UNIX file descriptor.  UNIX uses file descriptors to represent
both open files and open sockets.  A socket is an endpoint of
network communication.)  These \fIfileId\fR's are identical to
the identifiers manipulated by the \fBopen\fR, \fBread\fR,
\fBwrite\fR, \fBgets\fR, \fBputs\fR, and \fBclose\fR file
management commands of Tcl.

.SH REMOTE PROCEDURE CALL COMMANDS
.LP

The RPC commands of Tcl-DP are implemented on top of the
connection management commands listed above.  For speed, some of
the RPC commands are actually implemented in C instead of Tcl/Tk
scripting.  These RPC commands include \fBMakeRPCServer\fR,
\fBMakeRPCClient\fR, \fBRPC\fR, and \fBCloseRPC\fR.  The basic
\fBRPC\fR command has the following form:

\fBRPC\fR \fIfileId\fR ?\fIargs\fR?

Here is an example usage:

\fBRPC\fR file4 puts stdout "hello world"

The above RPC command will send its \fIargs\fR argument (the
string {puts stdout "hello world"}) to the connected remote
process, represented by file4.  The remote process will evaluate
the received \fIargs\fR string in its Tcl interpreter.  Any
return value will be sent back to the waiting RPC caller process.
Since all data and commands in Tcl/Tk are strings, there are no
data conversion or format problems.

.SH DISTRIBUTED OBJECT SUPPORT COMMANDS 
.LP

The distributed object commands of Tcl-DP are implemented on top
of the RPC commands described above.  An object in Tcl-DP is a
procedure, just like a widget instance of Tk is a procedure.  A
procedural object must take as its first argument a method
selector.  In other words, if \fIobject\fR is the name of a
procedural object:

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

The \fBslot-value\fR method must take one argument:

\fIobject\fB \fBslot-value\fR ?\fIslot\fR?

Objects are usually defined in C for speed but have a Tcl access
interface.  For example, Tk widgets are defined in C but have Tcl
access interfaces.  Objects can also be defined using Tcl/Tk
scripting only, as Tcl-DP supplies several utility procedures for
creating and manipulating objects with Tcl/Tk scripting only.
(See object(l).)

Once you have created an object, you may distribute that object
among several RPC connected processes.  Use the
\fBDistributeObject\fR and \fBUndistributeObject\fR commands.
(The graph of RPC connections among these processes must be
acyclic.)

Once an object is distributed, use the \fBsetf\fR and \fBgetf\fR
commands, not the \fBconfigure\fR and \fBslot-value\fR methods,
to access the slots of the distributed object.  These two
commands, \fBsetf\fR and \fBgetf\fR, replicate any slot value
changes to all processes where the object is distributed.

You may set triggers on any slot of any Tcl-DP distributed object
with the \fBSetTrigger\fR command.

.SH C INTERFACE
.LP

A C language interface is provided for several of the commands of
Tcl-DP.  See the man pages Address(3), Network(3), and RPC(3).

.SH SECURITY
.LP

The Tcl-DP package provides little support for access and
authentication security.  With Tcl-DP, any process can send any
Tcl/Tk command to any remote process.

A standard measure is to remove or undefine "dangerous" Tcl/Tk
commands, through the Tcl \fBrename\fR command.  Some of the Tcl
commands which you would probably want to remove include
\fBopen\fR, \fBexec\fR, \fBproc\fR, \fBconnect\fR (from Tcl-DP),
and lastly \fBrename\fR.  

The procedure \fBauto_load_all\fR is supplied, which will
\fBsource\fR all autoloading Tcl library scripts not already
\fBsource\fR'd.  Use \fBauto_load_all\fR before you undefine the
Tcl \fBproc\fR command.

.SH AUTHOR
Brian Smith (bsmith@cs.berkeley.edu)
.br
Steve Yen (syen@cs.berkeley.edu)
