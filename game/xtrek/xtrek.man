.TH XTREK 1 "1 November 1989" "X Version 11"
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
xtrek \- X based space warfare extravaganza
.SH SYNOPSIS
.sp
.B xtrek \fIhostname\fP
.SH DESCRIPTION
.sp
The \fIxtrek\fP game is a real time, interactive shoot-em up space
war game.
.PP
It is a fun and exciting pastime, which can waste hours of valuable
time, and comsume CPU minutes like there is no tomorrow.  It is also
useful in overloading the average ethernet to a horrifying extent.
.SH "X DEFAULTS"
.sp
The program understands the following class values:
.TP 8
.B "font (\fPclass\fB Font)"
Specifies the name of the normal font.  The default is ``vtsingle.''
.TP 8
.B "background (\fPclass\fB Background)"
Specifies the color to use for the background of the window.  The default is 
``white.''
.TP 8
.B "foreground (\fPclass\fB Foreground)"
Specifies the color to use for displaying text in the window.  Setting the
class name instead of the instance name is an easy way to have everything
that would normally appear in the "text" color change color.  The default
is ``black.''
.TP 8
.B "cursorColor (\fPclass\fB Foreground)"
Specifies the color to use for the text cursor.  The default is ``black.''
.TP 8
.B "reverseVideo (\fPclass\fB ReverseVideo)"
Specifies whether or not reverse video should be simulated.  The default is
``false.''
.SH COMMANDS

.I Mouse buttons:

.TP 8
LEFT     fire torpedo.
.TP
MIDDLE   fire phaser.
.TP
RIGHT    turn ship.
.TP 0
.I Keyboard commands:
.TP 2
\&.    Set fractional warp speed
.TP
k    Set course
.TP
p    Fire phaser
.TP
m    Drop a mine
.TP
n    Toggle bell.
.TP
t    Fire torpedo.
.TP
d    Detonate approaching enemy torpedos.
.TP
e    Detonate your torpedos.
.TP
D    Detonate your mines.
.TP
T    Turbo mode ON/OFF.
.TP
j    Jump (teleport).
.TP
+    Raise shields.
.TP
-    Lower shields.
.TP
u    Toggle shield UP/DOWN.
.TP
b    Bomb planet.
.TP
z    Beam up your armies.
.TP
x    Beam down armies.
.TP
f    Follow locked ship.
.TP
R    Repair your ship
.TP
o    Orbit planet
.TP
H    Bring on robot horde.
.TP
Q    Quit (self-destruct).
.TP
?    Repeat messages.
.TP
c    Raise/Lower cloak.
.TP
C    Coup a planet.
.TP
l    Lock on ship/planet.
.TP
@    Toggle co-pilot.
.TP
G    SETRHOSTILE_KEY
.TP
g    CLRRHOSTILE_KEY
.TP
*    Create easy-to-kill robot.
.TP
&    Create mongo-tough robot.
.TP
SP   Remove information windows.
.TP
L    List active players.
.TP
P    List planets.
.TP
S    List scores.
.TP
s    Create/Destroy status window.
.TP
U    Toggle "show shields" flag.
.TP
M    Toggle map mode.
.TP
N    Toggle name mode.
.TP
i    Information on planet.
.TP
h    Create/Destroy help window.
.TP
w    Show war window.

.SH ENVIRONMENT
.sp
.I Xtrek
uses the environment variable ``DISPLAY'' to specify
which bit map display terminal to use.
.SH "SEE ALSO"
.sp
xtrekd(6)
.br
.I ``Xtrek For Fun and Profit''
.br
.I ``Customizing Xtrek for Fun and Profit''
.SH BUGS
.sp
.PP
The message code is f**ked
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
