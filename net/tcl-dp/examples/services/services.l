'\"
'\" Copyright 1989 Regents of the University of California
'\" Permission to use, copy, modify, and distribute this
'\" documentation for any purpose and without fee is hereby
'\" granted, provided that this notice appears in all copies.
'\" The University of California makes no representations about
'\" the suitability of this material for any purpose.  It is
'\" provided "as is" without express or implied warranty.
'\" 
'\" $Header: /a/cvs/386BSD/ports/net/tcl-dp/examples/services/services.l,v 1.1 1993/09/01 00:46:30 jkh Exp $ SPRITE (Berkeley)
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
.HS Services cmds
.BS
.SH NAME
services server utility and RPC Tcl commands
.BE
.SH DESCRIPTION
.PP

The following Tcl/Tk procedures provide access to the services
server (through remote procedure call (RPC)) and require the
Tcl-DP (Tcl Distributed Programming) extensions to Tcl/Tk.
.SH UTILITY COMMANDS
.PP

The following Tcl/Tk commands access the services server through
implicit RPC.  These commands are syntactic sugar for the
explicit RPC commands listed in the next section:
.TP
\fBServiceFind \fIservice\fR

Returns the host network address and port number of the named,
advertised \fIservice\fR, if any.  The argument \fIservice\fR may
be a C-shell like pattern, such as "xtrek*".
.TP
\fBServiceAdvertise \fIservice host port\fR

Advertises the given named \fIservice\fR (running on the machine
with \fIhost\fR network address and \fIport\fR number) with the
services server.  This overwrites any previous advertisement of
the same \fIservice\fR name.
.TP 
\fBServiceUnadvertise \fIservice\fR

Unadvertises the given named \fIservice\fR in the services server.
.TP
\fBServiceSock\fR

Returns the \fIfileId\fR which represents the RPC connection to
the services server.  Using this \fIfileId\fR, a client can
\fBRPC\fR to the services server for direct communication.  The
above few Tcl commands, in fact, use \fBRPC\fR to access the
services server.
.SH RPC COMMANDS
.PP

Clients of the services server may use the follow commands,
through \fBRPC\fR, to query and to advertise on the services
server.
.TP
\fBListServices\fR

Returns a list of the names of all advertised services.
.TP
\fBListAliases\fR

Returns a list of the all aliases of advertised services.
.TP
\fBAliasService \fIservice alias\fR

Creates an \fIalias\fR for a given named, advertised \fIservice\fR.
.TP
\fBAdvertiseService \fIservice host port\fR

Advertises the given named \fIservice\fR (running on the machine
with \fIhost\fR network address and \fIport\fR number) with the
services server.  This overwrites any previous advertisement of
the same \fIservice\fR name.
.TP
\fBUnaliasService \fIalias\fR

Removes the \fIalias\fR in the services server.
.TP
\fBUnadvertiseService \fIservice\fR

Unadvertises the given named \fIservice\fR in the services server.
.TP
\fBFindExactService \fIservice\fR

Returns the host network address and port number of the advertised
service, if any, whose advertised name exactly matches the
argument \fIservice\fR.
.TP
\fBFindExactServiceNoCase \fIservice\fR

Returns the host network address and port number of the
advertised service, if any, whose advertised name exactly matches
the argument \fIservice\fR, ignoring case.
.TP
\fBFindMatchedService \fIservice\fR

Returns the host network address and port number of the advertised
service, if any, whose advertised name matches the C-shell like
pattern given by the argument \fIservice\fR.
.TP
\fBFindMatchedServiceNoCase \fIservice\fR

Returns the host network address and port number of the advertised
service, if any, whose advertised name matches the C-shell like
pattern given by the argument \fIservice\fR, ignoring case.
.TP
\fBFindService \fIservice\fR

Returns the host network address and port number of the
advertised service, if any, whose advertised name most exactly
matches the given argument \fIservice\fR.  \fBFindService\fR uses
the above \fBFindExactService\fR, \fBFindExactServiceNoCase\fR,
\fBFindMatchedService\fR, and \fBFindMatchedServiceNoCase\fR
commands to find the closest matching service.
.SH SEE ALSO
Tcl-DP, TCP/IP, services(1), accept, connect, listen,
/etc/services, /etc/cmcap
.SH AUTHOR
Steve Yen (syen@cs.berkeley.edu)

