'\"
'\" Copyright 1989 Regents of the University of California
'\" Permission to use, copy, modify, and distribute this
'\" documentation for any purpose and without fee is hereby
'\" granted, provided that this notice appears in all copies.
'\" The University of California makes no representations about
'\" the suitability of this material for any purpose.  It is
'\" provided "as is" without express or implied warranty.
'\" 
'\" $Header: /a/cvs/386BSD/ports/net/tcl-dp/man/manl/rpc.l,v 1.1 1993/09/01 00:46:32 jkh Exp $ SPRITE (Berkeley)
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
.HS RPC cmds
.BS
.SH NAME
RPC, RDO, MakeRPCClient, MakeRPCServer, CloseRPC, CancelRPC
.br
  \- Tcl-DP remote procedure call support
.BE
.SH DESCRIPTION
.LP

The commands described here are part of the Tcl-DP (Tcl
Distributed Programming) extension to Tcl/Tk.  These Tcl/Tk
commands provide support for Tcl/Tk remote procedure calls
(RPC's).
.SH COMMANDS
.TP
\fBMakeRPCServer \fIport\fR

\fBMakeRPCServer\fP arranges for the current Tcl/Tk interpreter
to accept RPC client connections.  Clients will connect to the
server at the given \fIport\fR number (see \fBMakeRPCClient\fP
below).  The \fIport\fR number should be an unused port number or
should be 0 if you want a port number automatically chosen for
you.  The port number used is returned.
.TP
\fBMakeRPCClient \fIhost\fR \fIport\fR

\fBMakeRPCClient\fP establishes an RPC connection to the RPC
server at the \fIport\fR number running on the machine with the
\fIhost\fR network address.  A \fIfileId\fR identifier
representing the RPC connection is returned.  This \fIfileId\fR
is refered to below as a \fIpeer\fR, since RPC connections are
peer to peer.
.TP
\fBCloseRPC \fIpeer\fR
.br
.sp
This command arranges a clean closing of an RPC connection, on
both sides of the connection.
.TP
\fBCancelRPC ?\fIpeer\fR? ?\fIpeer\fR? ...
.br
.sp
This command cancels current RPC invocations that are presently
waiting for a return value.  If no \fIpeer\fRs are specified,
then all current but waiting RPC invocations are canceled.
These cancelled RPC's will return an error.
.TP
\fBRPC \fIpeer\fR ?\fI-events events\fR? 
.br
?\fI-timeout millisecs\fR ??\fI-timeoutReturn callback\fR???
.br
\fIcommand\fR ?\fIargs ...\fR?

This command arranges for the Tcl/Tk \fIcommand\fR and its
\fIargs\fR to be evaluated in the remote Tcl/Tk interpreter,
using the RPC connection specified by \fIpeer\fR.  Before the
remote evaluation of \fIcommand\fR and \fIargs\fR, the global
variable \fBrpcFile\fR in the remote Tcl/Tk interpreter is bound
to the \fIpeer\fR whose request is being evaluated.  (Thus, in
your \fIcommand\fR, you can access the \fBrpcFile\fR variable to
figure out the RPC peer that originated the \fIcommand\fR.)

The \fI-events\fR flag to the \fBRPC\fR command lets you specify
which events should be handled while waiting for the return value
of a RPC.  The \fI-events\fR parameter should be a list of the
tokens \fBx\fR, \fBrpc\fR, \fBfile\fR, \fBtimer\fR, \fBidle\fR,
\fBall\fR, or \fBnone\fR, where each token represents an event
type.  Presently, a \fBrpc\fR token is the same as a \fBfile\fR
token.  The \fBnone\fR token is the default.  See
\fBTk_DoOneEvent\fR(3) for more information on these event types.

The \fI-timeout\fR flag to the \fBRPC\fR command lets you specify
an integer millisecond timeout value for the RPC, which by
default will return an error value if the timeout occurs.
Supplying a \fI-timeout\fR value of zero or less than zero means
that no timeout will be checked.  If you want to use timeouts,
you must also specify \fI-events\fR to be anything but none.

The \fI-timeoutReturn\fR callback, if specified, is evaluated
when a timeout occurs.  The callback is evaluated with the
\fIpeer\fR identifier appended.  If a timeout occurs and a
\fI-timeoutReturn\fR callback was specified, then the return
value of the callback is used as the return value of the
timed-out \fBRPC\fR.  Otherwise the timed-out RPC returns an
error.
.TP
\fBRDO \fIpeer\fR ?\fI-callback callback\fR? \fIcommand\fR ?\fIargs ...\fR?

This command arranges for \fIcommand\fR and its \fIargs\fR to be
remotely evaluated in the Tcl/Tk interpreter whose connection is
specified by \fIpeer\fR argument.  Unlike the \fBRPC\fR command,
the \fBRDO\fR command does not wait for a return value to be
returned by the remote Tcl/Tk interpreter, but returns
immediately.  Before the remote evaluation, the global variable
\fBrpcFile\fR in the remote Tcl/Tk interpreter is set as
described above for the \fBRPC\fR command.

You can use the \fIcallback\fR parameter of \fBRDO\fR to
asynchronously retrieve the value of the remote evaluation.  If
specified, the \fIcallback\fR command appended with the return
value of the remote \fIcommand\fR evaluation is evaluated in the
local Tcl/Tk interpreter.
.TP
\fBRPROC \fIname\fR \fIarguments\fR \fIbody\fR

The RPROC command is the same as the \fBproc\fR command -- it
defines a new Tcl procedure.  It is simply syntactic sugar that
helps you tell if a given procedure is safe to be remotely
invoked by RPC clients.
.SH EXAMPLES
.PP
The following example creates an RPC server to return unique 
integer identifiers.  
On the host zonker (the server):
.DS
	MakeRPCServer 4545

	set myId 0
	proc GetId {} {global myId; incr myId; return $myId}
.DE
.PP
On host linus (the client):
.DS
	set server [MakeRPCClient zonker 4545]
	RPC $server GetId
.DE

.SH "SEE ALSO"
.PP
Tcl-DP, connect

.SH AUTHOR
Brian Smith (bsmith@cs.berkeley.edu)
.br
Steve Yen (syen@cs.berkeley.edu)
