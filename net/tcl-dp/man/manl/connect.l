'\"
'\" Copyright 1989 Regents of the University of California
'\" Permission to use, copy, modify, and distribute this
'\" documentation for any purpose and without fee is hereby
'\" granted, provided that this notice appears in all copies.
'\" The University of California makes no representations about
'\" the suitability of this material for any purpose.  It is
'\" provided "as is" without express or implied warranty.
'\" 
'\" $Header: /a/cvs/386BSD/ports/net/tcl-dp/man/manl/connect.l,v 1.1 1993/09/01 00:46:32 jkh Exp $ SPRITE (Berkeley)
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
.HS connect cmds
.BS
.SH NAME
connect, shutdown, accept, send, receive, sendTo, receiveFrom,
packetSend, packetReceive 
.br
  \- Tcl-DP network connection management commands
.BE
.SH DESCRIPTION
.LP

Several Tcl/Tk commands for managing basic network connections
are described in this man page.  These commands are part of the
Tcl-DP (Tcl Distributed Programming) extension to Tcl/Tk and
provide basically the same level of service to Tcl/Tk as the UNIX
C \fBsocket\fR(2) related system calls.

These connection management commands return and take as arguments
identifiers called \fIfileId\fR's.  (A \fIfileId\fR represents a
UNIX file descriptor.  UNIX uses file descriptors to represent
both open files and open sockets.  A socket is an endpoint of
network communication.)  These \fIfileId\fR's are identical to
the identifiers manipulated by the \fBopen\fR, \fBread\fR,
\fBwrite\fR, \fBgets\fR, \fBputs\fR, and \fBclose\fR file
management commands of Tcl.
.SH COMMANDS
.TP
\fBconnect \fIhost\fR \fIport\fR

This command creates a TCP protocol INET socket and connects it
to a remote server process.  The remote server process should be
running on the machine given by the \fIhost\fR network address
and should be listening for and accepting client connections on
the given \fIport\fR number.  This command returns a list of two
values: a \fIfileId\fR to represent the created, connected
socket and the port number used for the connection.
.TP
\fBconnect \fI-server\fR \fIhost\fR \fIport\fR

If the \fI-server\fR flag is specified to the \fBconnect\fR
command, then a listening (server) socket is created.  This
listening socket will listen for client connections on the
specified \fIport\fR number.  The listening socket will become
readable whenever a client process attempts a connection.  The
creator of the listening socket may then \fBaccept\fR the client
connection.  (See the \fBaccept\fR command below.  Also see
\fBfilehandler\fR man page for testing when a socket becomes
readable.)

The \fIport\fR number argument must be an unused port number.  If
the \fIport\fR number argument is empty or 0, the \fBconnect\fR
command will automatically select an unused port number.

If the \fIhost\fR argument is an empty string, then any client
process may attempt to connect to the listening socket.
Otherwise, only processes running on the machine with the
\fIhost\fR network address may attempt to connect to listening
socket.

This \fBconnect\fR command with the \fI-server\fR flag returns a
list of two values.  The first value is the \fIfileId\fR that
represents the created listening socket.  The second value is the
port number that the socket is listening to.
.TP
\fBconnect \fI-server\fR \fIpath\fR

This command creates up a UNIX domain listening socket.  This
command is identical to the one above except that a UNIX domain
socket is created using an address specified by \fIpath\fR.
.TP
\fBconnect \fI-udp\fR \fIport\fR

If the \fI-udp\fR flag is specified, then the \fBconnect\fR
command creates a UDP protocol INET socket (a connectionless
socket).  Since this socket is connectionless, only the
\fBsendTo\fR and \fBreceiveFrom\fR commands described below may
be used with this socket to transmit and receive data.

If the \fIport\fR argument is empty or 0, the \fBconnect\fR
command will select an unused port number for you.  This command
returns a list containing a \fIfileId\fR (that represents the
created socket) and the \fIport\fR number (where the socket will
receive and transmit data using the UDP protocol).
.TP
\fBaccept \fIfileId\fR

The \fIfileId\fR argument must represent a listening (server)
socket (e.g., a \fIfileId\fR returned by the \fBconnect\fR
command that was called with the \fI-server\fR flag).  If this
listening socket is readable (i.e., a client process is
attempting to connect), the \fBaccept\fR command creates a new
socket that is connected to the client process.  If the listening
socket is not readable, th \fBaccept\fR command will block until
a client process attempts to connect to the listening socket.
The listening socket (represented by the \fIfileId\fR argument)
will remain a listening socket.  This command returns a new
\fIfileId\fR to represent the newly created socket that is
connected to the accepted client process.
.TP
\fBsend\fR \fIfileId\fR \fImessage\fR

Sends the string \fImessage\fR through the socket (\fIfileId\fR)
to the connected, remote process.  This command is preferred to
Tcl \fBputs\fR command for sending messages through network
connections because it is optimized for network operations.  If
more than one \fImessage\fR is sent before the receiver process
can process (using \fBreceive\fR or \fBgets\fR) the first sent
message, the boundaries between messages is lost.  If this is
undesirable, use the \fBpacketSend\fR and \fBpacketReceive\fR
commands, instead of the \fBsend\fR and \fBreceive\fR commands.
It is recommended that \fBsend\fR and \fBpacketSend\fR not be
both used on the same \fIfileId\fR channel.
.TP
\fBreceive\fR \fIfileId\fR

Receives a message from the socket (\fIfileId\fR) which was sent
from connected, remote process which used the \fBsend\fR command.
The message received is returned.  If the socket is not readable,
this command will block.  It is recommended that \fBgets\fR and
\fBputs\fR not be mixed with \fBsend\fR and \fBreceive\fR on the
same \fIfileId\fR channel.
.TP
\fBsendTo\fR \fIfileId\fR \fImessage\fR \fIaddr\fR

Sends the string \fImessage\fR to the process at the destination
address specified by the \fIaddr\fR handle.  The \fIaddr\fR
handle must be created using the \fBaddress\fR command or taken
from the return value of the \fBreceiveFrom\fR command.  The
\fIfileId\fR argument must represent a connectionless socket
(i.e., created using the \fBconnect\fR command using the
\fI-udp\fR option).  The \fBsendTo\fR command uses the
connectionless socket, \fIfileId\fR, as the transmitting socket.
.TP
\fBreceiveFrom\fR \fIfileId\fR

Receives a message that was sent from another process that used
the \fBsendTo\fR command.  A list of two values is returned.  The
first element is the address (an \fIaddr\fR handle) of the
sending process, and the second element is the message received.
If the socket is not readable, this command will block.  The
\fIfileId\fR argument must represent a connectionless socket
(i.e., created using the \fBconnect\fR command using the
\fI-udp\fR option).  The \fBreceiveFrom\fR command uses the
connectionless socket, \fIfileId\fR, as the reception socket.
.TP
\fBpacketSend\fR \fIfileId\fR \fImessage\fR

Sends the string \fImessage\fR through the socket (\fIfileId\fR)
to the connected, remote process.  This command is similar to
\fBsend\fR, except that message boundaries are preserved when
received at the far end with the \fBpacketReceive\fR command.  It
is recommended that \fBsend\fR and \fBpacketSend\fR not be both
used on the same channel.
.TP
\fBpacketReceive\fR \fIfileId ?-noblock?\fR

Receives a single message from the connection (represented by the
\fIfileId\fR).  The connected, remote process which sent the
message must use the \fBpacketSend\fR command.  Message
boundaries are preserved when more than one message is in the
receive buffer.  If the argument \fI-noblock\fR is supplied, the
receive is done without blocking.  The message received in is
returned.  It is recommended that \fBreceive\fR and
\fBpacketReceive\fR not be both used on the same channel.
.TP 
\fBshutdown \fIfileId\fR \fIcode\fR

Shutdown a socket for reading, writing, or both.  This command is
not the same as \fBclose\fR, which you should use for truly
closing the socket.  The \fIfileId\fR argument must represent a
file descriptor that represents a socket, not an open file.

If \fIcode\fR is \fB"0"\fR, \fB"read"\fR or \fB"receives"\fR then
further reception of data from \fIfileId\fR will be disabled.

If \fIcode\fR is \fB"1"\fR, \fB"write"\fR or \fB"sends"\fR, then
further sending of data using \fIfileId\fR will be disabled (and
an EOF will be automatically sent down the socket).

If \fIcode\fR is \fB"2"\fR, \fB"all"\fR or \fB"both"\fR, then
further reception and sending of data on \fIfileId\fR will be
disabled.

As a side effect, the appropriate filehandlers previously
installed on the \fIfileId\fR will be deleted.  (See the
\fBfilehandler\fR man page.)
.SH SEE ALSO
Tcl-DP, socket(2), accept(2), connect(2), listen(2), address(l),
Tcl, filehandler(l)
.SH AUTHOR
Pekka Nikander, Telecom Finland (Pekka.Nikander@ajk.tele.fi)
.br
Extended by Tim MacKenzie (tym@dibbler.cs.monash.edu.au)
.br
Further enhancements by Brian Smith (bsmith@cs.berkeley.edu)
.br
and Steve Yen (syen@cs.berkeley.edu)
