.TH XTREKD 1 "1 Noveber 1989" "X Version 11"
.de Ds
.nf
.\\$1D \\$2 \\$1
.ft 1
.ps \\n(PS
.\".if \\n(VS>=40 .vs \\n(VSu
.\".if \\n(VS<=39 .vs \\n(VSp
..
.de De
.ce 0
.if \\n(BD .DF
.nr BD 0
.in \\n(OIu
.if \\n(TM .ls 2
.sp \\n(DDu
.fi
..
.SH NAME
xtrekd \- X based space warfare extravaganza
.SH SYNOPSIS
.sp
.B xtrekd \fIconfig_file\fP
.SH DESCRIPTION
.sp
\fIxtrekd\fP is the
.I xtrek
central server.  It will run on a central compute server, and communicate
with
.I xtrek
processes running on remote computers with bitmap displays.
.PP
At startup, \fIxtrekd\fP will initialize a galaxy to be used as the playing
field.  If no config file is specified on the command line, \fIxtrekd\fP
will use a built-in default scenario.  Specifying a config file will
direct \fIxtrekd\fP to initialize the galaxy based on the information
in the config file.
.SH "SEE ALSO"
.sp
xtrek(6)
.br
.I ``Xtrek For Fun and Profit''
.br
.I ``Customizing Xtrek for Fun and Profit''
.SH BUGS
.sp
.PP
The message code don't work right.
.SH COPYRIGHT
.sp
Copyright 1988, Massachusetts Institute of Technology.
.br
See \fIX(1)\fP for a full statement of rights and permissions.
.SH AUTHORS
Far too many people, including:
.sp
Dave Gagne (University of BC)
.br
Mike Bolotski (University of BC)
