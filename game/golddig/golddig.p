.TH GOLDDIG P "September 29 1989"
.UC 4
.SH NAME
golddig - Getting the Gold and Avoiding Death
.SH
.B golddig
[
.B \-l
.I level
]
[
.B \-s
.I speed
]
[
.I world
]
.br
.B makelev
.B \-l
.I level
[
.B \-w
.I width
]
[
.B \-h
.I height
]
[
.I world
]
.SH DESCRIPTION
.PP
.I golddig
is a fast action game designed for use with the X window system.
.I makelev
is used to create and edit new levels which can be used by
.I golddig.
.PP
The goal in
.I golddig
is to pick up all the gold pieces and then go to the next level
without getting eaten by one of the bad guys.  The gold pieces are
distinctive small circular coins.  After picking up all of the gold,
the player must get to an open door or the top row of the level.
After accomplishing this, the game will start the next level.
.PP
The
.B "\-l"
option sets the level number for editing or for starting the game.
This may be any integer greater than 0.  By default, the game starts
at level 1.  The
.B "\-s"
options sets the game speed.  A value of 1 is very slow and a value of
10 is fast.  The default value is 5.  The
.B "\-w"
and
.B "\-h"
options set the width and height of the level when creating a new
level.  The
.B world
option is used to select a world name.  A world is a distinct set of
numbered levels.  Any valid file name may be used as the world name.
Different worlds may be used to allow users to control private sets of
levels.
.PP
When playing
.I golddig
the following keyboard commands may be used:
.PP
<space>,R11 \- stop
.br
a,j,left arrow \- move left
.br
d,l,right arrow \- move right
.br
w,i,up arrow \- move up
.br
s,k,down arrow \- move down
.br
z,<,q,u,R13 \- make hole left
.br
x,>,e,o,R15 \- make hole right
.br
r,y,R7 \- put down any held item
.br
1\-9 \- change the game speed during the game
.PP
In addition, some control keys can be used:
.PP
^S,^Z \- freeze the game
.br
^Q,^Y \- reactivate the game
.br
^C,^U,^\\ \- kill the game
.br
^R - redraw the level
.PP
The mouse must be placed in the game window to use the keyboard
commands.  When a level starts, everything is frozen, and any key will
start the game.
.PP
.I makelev
is used as a graphics editor.  Each level is divided into a grid of
blocks.  The blocks are "painted" by pressing the left mouse button
down and moving the mouse.  The right mouse button always erases
blocks.  A new block type is selected by a keyboard
key.  Currently, the following block types are supported:
.PP
<space> \- draw spaces
.br
! \- draw escapes
.br
# \- draw walls
.br
$ \- draw golds
.br
- \- draw ropes
.br
< \- draw left arrows
.br
= \- draw tubes
.br
> \- draw right arrows
.br
@ \- draw stones
.br
^ \- draw anti-spaces
.br
a \- draw armors
.br
b \- draw bad guys
.br
c \- draw chutes
.br
d \- draw down arrows
.br
e \- draw reverse monsters
.br
f \- draw up arrows
.br
g \- draw ghost bricks
.br
i \- draw invisible blocks
.br
j \- draw jump pads
.br
k \- draw kill zones
.br
l \- draw power shovels
.br
m \- draw super jump pads
.br
n \- draw nuclear shovels
.br
o \- draw anchors
.br
p \- draw players
.br
q \- draw speed boots
.br
r \- draw parachutes
.br
s \- draw step ladders
.br
t \- draw teleporters
.br
u \- draw leave levels
.br
v \- draw vertical ropes
.br
w \- draw windows
.br
x \- draw extra bricks
.br
y \- draw heavy fogs
.br
z \- draw time stops
.br
| \- draw ladders
.br
~ \- draw portable ropes
.PP
A player block sets where the player begins in the level.  If more
than one is used in a level, all but one will be ignored.  Bad guy
blocks set where the bad guys begin the level.  There is a limit to
the total number of bad guys, and that number is compiled into the game.
Pressing control W inside the editor window will stop editing
and save the level.  Pressing control C quits without saving.
Pressing control E will erase the contents of the
editor.  The properties of the rest of the block can be
determined with experimentation.
.PP
A really good level has several properties.  The first important
property is efficiency.  A good level makes careful use of all of the
available space unless it is trying to achieve a specific effect.
Another critical property is fairness.  A player should always be
forewarned or have an escape if trouble comes up.  Nothing is more
irritating than a killer trap where the user had no way of guessing that
it was coming up.  It is also important to avoid tedious and repetative
levels.  Really difficult traps should be rewarded with lots of gold.
.SH AUTHORS
Alex Siegel at Cornell University during September of 1989.
.SH
/usr/games/golddig - golddig executable
.br
/usr/games/makelev - level editor executable
.br
/usr/games/lib/golddig - directory for storing files which are private to
.I golddig
.br
/usr/games/lib/golddig/scores - file for storing high score list
.br
/usr/games/lib/golddig/default - default level for use when no
generated level is available
.br
/usr/games/lib/golddig/goldlev### - default levels used when no world
name is specified
.SH BUGS
This game is the first totally bug free program ever written.
