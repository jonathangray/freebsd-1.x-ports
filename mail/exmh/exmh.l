'\"
'\" 
'\" $Header: /a/cvs/386BSD/ports/mail/exmh/exmh.l,v 1.1 1994/05/04 15:04:34 jkh Exp $ SPRITE (Berkeley)
'/" 
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
.\" .if '\\$2'cmds'       .TH \\$1 1 \\$3 \\$4
.\" .if '\\$2'lib'        .TH \\$1 3 \\$3 \\$4
.\" .if '\\$2'tcl'        .TH \\$1 3 \\$3 \\$4
.\" .if '\\$2'tk'         .TH \\$1 3 \\$3 \\$4
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
.TH "EXMH" 1 "April 7, 1994" "Exmh"
.HS
.BS
'\" Note:  do not modify the .SH NAME line immediately below!
.SH NAME
exmh \- A TK-based front-end for the MH mail system.
'
.SH SYNOPSIS
\fBexmh\fR ?\fIoptions\fR?\fR ?\fIfolder\fR?\fR
.SH OPTIONS
.IP "\fB\-bgAction \fIaction\fR" 15
Override background processing setting.
Valid actions are: off, count, msgchk, flist, and inc.
.IP "\fB\-bgPeriod \fIminutes\fR" 15
Override background period setting.
.IP "\fB\-geometry \fIgeometry\fR" 15
Initial geometry to use for the window.
.IP "\fB\-iconposition \fIgeometry\fR" 15
Initial position to use for the icon in the form +\fIxoff\fP+\fIyoff\fP.
You can use minus signs instead of + to position relative to the right
and bottom of the screen.
.IP "\fB\-iconic\fR" 15
Begin execution iconic.
.BE

.SH DESCRIPTION
.PP
\fBExmh\fP provides a user interface to the MH mail system.  For info
on MH, start by reading the \fBmh\fP man page.
Exmh provides an interface to MH commands that let you do
various things like read mail,
manage a set of folders, reply, forward and compose new messages.
As well as providing this (and other) MH functionality, \fBexmh\fP
has a number of other features:
.PP
MIME support.  Exmh parses MIME (Multipurpose Internet Mail Extensions)
messages.  It displays richtext and enriched text directly in its viewer.
A pop-up menu obtained with the right mouse button lets you save or
process the contents of MIME messages of all types.
This uses the metamail package.
The built-in editor lets you compose enriched text messages.
.PP
A display of your folders that appears as a set of labels,
one for each folder.  The folder labels are highlighted to
indicate the current folder, the target folder for moves,
folders with unread mail in them,
and folders with nested folders under them.
\fBExmh\fP supports arbitrarily nested folder structures.
.PP
The scan listing (folder table-of-contents)
is also highlighted to indicate the current message,
unread messages, and messages marked for move or delete.
.PP
Clever scan caching.  MH users know that scan is slow, so
\fBexmh\fP tries hard to cache the current state of the folder to
avoid scanning.  Moves and deletes within \fBexmh\fP do not
invalidate the cache, and background incs that add new messages
are handled by merging them into the scan listing.  The
scan cache is compatible with xmh.
.PP
Facesaver bitmap display.  If you have a facesaver database
on your system, \fBexmh\fP displays the bitmap face of the person
that sent the current message (or their organization).
(You can pick up the facesaver database from ftp.uu.net
under /faces/bundled or
the master site at cs.indiana.edu:/pub/faces)
.PP
Background processing.  You can set \fBexmh\fP to run inc periodically,
check for new mesages arriving asynchronously in folders,
run the MH msgchk program,
or count up the messages in your mail spool file.
.PP
Various inc styles.  Exmh knows about four styles of inc usage:
The default is to Inc from your spool file to your inbox folder ("inbox").
You can also Inc from a set of dropboxes as specified by
your ~/.xmhcheck file ("multidrop").
The dropboxes can be POP mailhosts, or UNIX files.
If you have some external agent that delivers mail into your folders
for you, then all you do not need inc directly ("none").
Finally, exmh can do maildelivery for you (eliminating the need
for an external agent).  In this case ("presort"), \fBexmh\fP
will inc from your spool file into a temporary folder and then
run the MH \fBslocal\fP filtering hook on each new message.
.PP
Unseen folder display.  If you use a system that inc's into
various folders, not just inbox, then you have the problem
of finding what folders have unseen messages in them.  \fBexmh\fP
provides feedback in the folder pane so you can tell
which folders have new messages.
Also, the Next command will chain to the next folder with
unread messages if you are at the end of the current folder.
.PP
Fast searching over the current folder listing and current message body.
.PP
An interface to MH pick that lets you select messages by patterns in the
header fields, by date, or by sequence name.
.PP
Editor interface.  You can hook \fBexmh\fP to your favorite editor
using the \fBexmh-async\fP script.  Or, TCL-based editors such
as \fBmxedit\fP can interact with \fBexmh\fP directly.
As the default, a simple built-in editor named \fBsedit\fP is provided.
.PP
Keybinding User Interface.  Bindings for common editting and cursor
motion commands are exposed in a UI that lets you choose keystroke
sequences to match your expectations.
.PP
User programming.  \fBExmh\fP is structured on the assumption
you'll want to play with it a bit.  Its functionality is split into
many smallish modules that you can override through a personal
library of TCL routines.  Other aspects are controlled by
a per-user Xresources file, ~/.exmh-defaults.
The Preferences dialog of \fBexmh\fP
allows you to set most, but not all, of these resources.
There is a section all about tweaking \fBexmh\fP
towards the end of this man page.
.SH GETTING STARTED
.PP
Mostly likely you are already an MH user.  If not, read
the \fBmh\fP man page, and get your MH environment set up by running
its \fBinc\fP command.  (This is a requirement.  \fBexmh\fP will
exit if you do not have an MH environment setup.)
.PP
\fBExmh\fP depends on two .mh_profile
entries, Draft-Folder and Unseen-Sequence, as well as
the basic Path entry.  Dialog boxes will appear if these
entries are not present, and \fBexmh\fP can set them up
for you if you ok it.  The Draft-Folder is used to keep
pending drafts of messages, and it is typically named
"drafts".  The Unseen-Sequence entry defines the name of
the sequence for unread messages.  \fBExmh\fP uses this
sequence in order to highlight unread messages and folders
containing unread messages.
.PP
Additional .mh_profile entries can tune \fBexmh\fP.
They are described in the section on MH below.
.PP
The first time you run \fBexmh\fP, go ahead and open the
Preferences dialog by clicking on the Preferences button.
Go through the various preference options, clicking on
the label to get a more detailed description of it.
There is also a Help button inside the Preferences dialog
that explains how to set things up.  The important things
to get set right are your editor choice and the method
you use to incorporate new mail (more on this below).
You can also arrange
for \fBexmh\fP to periodically inc messages.
.PP
\fBExmh\fP uses MH commands to scan folders, incorporate mail,
and mark messages.  However, it optimizes these calls by
postponing or batching them.  In particular, the message sequence
information is only updated when you change folders,
incorporate new mail, sort or pack a folder,
force a rescan, or commit message changes.
.PP
The Delete and Move commands mark messages as needing these
operations.  The operations are not performed until you
commit them by clicking the Commit button (or pressing <Return>).
Delete results in a call to \fBrmm\fP.  If you want deleted messages
to be refiled into a special folder (e.g. +wastebasket or +deleted),
you'll have to define a shell script and register that as your
rmmproc in your .mh_profile.
.SH THE EXMH DISPLAY
.PP
Exmh displays three panes of information.
The top pane has a label for each of your MH folders.
Left click on a folder to view the
messages in it.  Right click on a folder moves the current message(s)
to the folder and makes the folder the target for future moves.
The folder display is highlighted to reflect the state of folders.
.PP
On color displays:
.DS
red - the current folder.
blue - a folder with unseen messages.
yellow - the target folder for move operations.
.DE
.PP
On monochrome displays:
.DS
black - the current folder.
bold outline - a folder with unseen messages.
stippled background - the target folder for move operations.
.DE
.PP
Folders that have nested folders under them are
highlighted by a shadow box.
There are two ways to view the contents of a nested
folder.  Choose the one you like via Preferences.
The default ("navbutton") is that a middle click on a folder label
displays a popup window that contains labels for
the nested folders.  This works recursively.
You can also have the popup appear when you enter the label ("enter").
Alternatively you can choose to
change the display to those folders inside
the nested folder ("redisplay").
In this mode, you can go back up a level by middle clicking
on the label "..".
.PP
The button assignments can be changed by setting Xresources, hence the
term "navbutton" instead of "middle".  See the section on Xresources
in this man page for details.
.PP
The middle and bottom panes are text windows.
Each of these can be scrolled by using the scrollbar
or by dragging the display with the middle mouse button.
An Xresource item lets you put the scrollbars on the
left side instead of the right, which is the default.
.PP
The middle pane lists the messages in the current folder,
which comes from the output of the MH scan command.
This pane is referred to as the FTOC (folder-table-of-contents)
pane in the find dialog.
On color displays:
.DS
    Red - the current message.
    Blue - unread messages.
    Grey background - messages marked for deletion.
    Yellow background - message marked for refile to another folder.
.DE
.PP
On monochrome displays:
.DS
    Reverse video - the current message.
    Underlined - unread messages.
    Cross-hatched background - messages marked for deletion.
    Stippled background - message marked for refile to another folder.
.DE
.PP
Left clicking on an entry causes the message to be displayed
in the third, or bottom pane.
Or, if you drag out a selection with the left button,
a range of message will be highlighted.
You can modify this range selection with shift-left clicks or drags.
Right clicking on an entry makes it current,
but does not display it.
The middle button is used to scroll the window;
drag the display up or down while holding down the middle button.
.PP
MIME messages are indicated by displaying their content-type in the
status line (e.g. text/plain or multipart/digest).
A popup menu is available to process MIME messages, such as saving
their contents or processing the contents with an external viewer.
Press the right mouse button in the message display area to obtain this menu.
For multipart messages,
the menu changes depending on what part of the message you are
looking at.  See also the Preferences entry for MIME for some
parameters you can adjust.
.PP
After you view a message
you will typically want to leave it alone and just
view the next message (click Next or type 'n'),
mark the message for deletion (click Delete or type 'd'),
or refile the message into the target folder
(click Move or type 'm').
You must have first selected a target folder before Move will work.
.PP
You can display a message in a new top-level window in order to save
it around on your display.  Use the Clip operation under the message More...
menu.  This will copy the current message into a new window.
.SH EXMH MAIN BUTTONS
.PP
There are three sets of buttons in exmh, the
main, or top row, folder operations, and message operations.
The main set has global commands: Log, Help, Color Codes, Bind, Preferences, and Quit.
.IP Log
This displays a debugging log, if you enable this in Preferences.
.IP Help
This provides a useful sub-set of this man page.
.IP "Color Codes"
This provides a highlighting key for color or monochrome screens.
.IP Bind
This displays a dialog that lets you define some basic key bindings.
These bindings result in class definitions for the
Entry and Text widget classes.  By default, \fBexmh\fP starts
out with roughly emacs-like bindings.
.IP Preferences
This displays a dialog that lets you customize \fBexmh\fP.
There is also a different Help button in the Preference dialog that
explains how to make selections and get more info.  In particular,
you can click on the descriptive label for an item to get
a popup with more detailed information on the preference item.
.SH FOLDER BUTTONS
.PP
The middle set of buttons has folder commands:
Pick, New, Find, Flist, Inc, Commit, and More...
.IP Pick
This provides an interface to the MH \fBpick\fP command.  The dialog initially
displays entry widgets labeled Subject, From, To, Cc, Before, and After.
You can enter a pattern for any of these, and \fBpick\fP selects messages that
satisfy the logical AND of these pattern specifications.
Note that the Before and After patterns are date-specific, and \fBpick\fP
understands many forms of date patterns.  Read the MH man page on \fBpick\fP
for the complete story.  You can build up more complex logical search
expressions by clicking the Or button.  The Messages entry lets you limit
\fBpick\fP to a specific set of messages. In particular, you can specify
a sequence name (e.g., "unseen"), and have pick find all messages with that
sequence identifier.  After the pick completes, a range of messages is
selected that satifies the search, and you can operate on these messages
as described below.
There are two check buttons in the Pick dialog that must be set properly
*before* you make the pick: "Add to Sel", and "New FTOC".
If you choose "Add to Sel", then the picked messages will be added to
the currently selected set.  Otherwise a new selection is made.
If you choose "New FTOC", then the selected messages will replace the current
folder table of contents display.
After a pick selection has been made, you can click
"Mark Seen" to remove the messages from the unseen sequence.
Pick uses the MH pick program, which runs a bit slowly,
so have patience.
.IP New
This pops up a dialog box from which you can create new folders.
.IP Find
Find causes a search dialog to appear.  You can enter a string
to find in either the folder listing (FTOC) or message body (Msg).
There are radio buttons that select these two choices.
<Return> causes the search to
begin.  <Control-s> and <Control-r> cause subsequent forward
and reverse searches, respectively.
This searching is done by scanning the FTOC display or the message body
display, so it only hits on the current folder scan output and the
current message body.  Use Pick for more general searching.
.IP Flist
The Flist button refreshes the color feedback of folder buttons
so you can tell which folders have unseen messages in them.
.IP Inc
The Inc button will incorporate new mail from your spool file
into your mail folders.
This command is also bound to the keystroke 'i'.
.IP Commit
The Commit button causes pending delete and move operations to occur.
This command is also bound to keystroke <Return>.
Know that messages marked for deletion are really removed by
means of the MH rmm command.
Unlike \fBxmh\fP, \fPexmh\fP requires that you commit changes before
changing folders and doing other operations that modify the scan listing
(e.g., Pack, Sort, Rescan).
However, you can set an Xresource (*autoCommit) to have exmh automatically
commit changes when needed.
.IP More...
This is a menu with more folder operations in it.
.IP "Rescan folder"
Rescan forces a rescan of the folder contents.  Exmh maintains a
cache of the scan output that could get confused.  The Rescan
button forces a full scan.  If you just want to scan the new
messages that have arrived in the folder you are currently
reading, it is more efficient to click on the folder's label
in the top pane.  This will invoke a scan merging procedure
that attempts to preserve the cached results of previous scans.
You should only need Rescan when you have reason to believe
that \fBexmh\fP is out of sync.
.IP "Pack folder"
This will renumber messages in the current folder
to eliminate any gaps in the number sequence.
.IP "Sort folder"
This will sort the current folder by message date.
.IP "Find all folders"
This refreshes the display of folder buttons,
and it will pick up any new folders you have created.
This currently uses the MH command "folders -fast -recurse",
which can be slow.
The results are saved, so you should not
need to wait for this scanning unless you change your folder
structure.  (You can also edit the .folders file in your
MH mail directory.)  The exmh distribution comes with a two-line patch
for the MH folders program that speeds it up considerably.
.IP "Delete folder"
This lets you delete a folder.  The folder must be empty, except
for .xmhcache, .mh_sequences, and any .nfs junk files.
.IP "Catchup Unseen"
This clears the unseen sequence from the current folder.
.IP "Update scan caches"
This runs another wish script in the background to update any
scan caches that are out of date.
.IP "Update all scan caches"
This runs another wish script in the background to update all the
scan caches.
.SH MESSAGE BUTTONS
.PP
The last set of commands are message operations.
Several of these work in a batch fashion on all the
messages in a selected set.
These include Delete, Move, Forward, Unmark, and Print.
Some of these buttons and menu entries
are disabled if there is no current message.
Disabled actions have their text greyed out.
.IP Forward
Forward the current message(s).
Also keystroke 'f'.
.IP ReplyAll
Reply to the current message, including on the cc line everyone that
got the original message (-cc cc -cc to).
Also keystroke 'R'.
.IP reply
Reply to the current message, only to the person that send the original.
Also keystroke 'r'.  The lower case reply in the button label is intentional
to help you remember the difference between the 'r' and 'R' keyboard commands.
.IP Send
Compose a mail message.  Chose the editor to use via the Preferences dialog. Also keystroke 'c' (for "compose").
.IP EDIT
The Send button is changed into the EDIT button when you enter
the drafts folder.  This button brings your editor up on the current
draft message.  After exiting the editor you can send or abort the message.
.IP Prev
Move to the previous message.
Also keystroke 'p'.  Keystroke 'P' changes the current message
without displaying it.  Use 's' to display such a message.
.IP Next
Move to the next message.  This will chain to the next folder
with unread messages if you are at the end of the current folder.
Also keystroke 'n'.  Keystroke 'N' changes the current message
without displaying it.
.IP Delete
Mark the current message(s) for deletion and go to the next message.
Also keystroke 'd'.  If you change your mind, select the message
again and "Unmark" it via the entry under the message More... menu.
.IP Move
Mark the selected message(s) to be moved to the target folder and go to the next message.  Also keystroke 'm'.
Note that selecting a target folder also moves the current message(s).
So, you use the Move button if the correct target folder is
already selected.
If you make a mistake and want to move the
message to a different folder, just select it again and right click on
the proper destination folder.  If you don't want to move it at all,
then "Unmark" the message.
.IP More...
This is a menu of more message operations.
.IP Print
Print the current message, or the selected set of messages.  The print
command is defined via the Preferences dialog.
.IP Unmark
Cancel a move or delete mark on the selected message(s).  Also keystroke 'u'.
.IP Clip
Display a new toplevel window containing the current message.
.IP "First Unseen"
Display the first unseen message, if any.  Also keystroke 'U'.
.IP "Burst Digest"
Use the MH burst command to split up a digest message into its individual
messages.  In addition, the new messages are added to the unseen sequence.
You can control whether or not the orignal message is replaced with just
the digest header by specifying burst options in your .mh_profile. (E.g., "burst: -inplace").
.IP "Redistribute"
This uses the MH dist command to resend a message to a different recipient.
.IP "Save to file"
This simply copies the current message(s) to a file selected via the file
selector dialog.
.SH INCORPORATING NEW MAIL
.PP
Exmh knows about four styles of Inc,
which you choose via the Preferences dialog:
.IP inbox
Inc moves new messages into your inbox folder.  This is the default.
.IP multidrop
This assumes that you use MH maildelivery facilities to
put new messages into a set of dropboxes (inbox-like files)
that correspond to different folders.  In addition,
a dropbox can be a POP host.  In this case,
Inc moves messages from the dropboxes into folders
as specified by your ~/.xmhcheck file.  Example .maildelivery
and .xmhcheck files appear at the end of this man page.
.IP presort
Inc moves new messages directly into various folders.
This is similar to what you may be doing already with
an external agent.  I built this into exmh in order
to elminate the need for such an agent in my somewhat wierd environment,
but you can continue to use your setup if you like.
The way presort works is to inc from your spool file
into a temporary folder (MyIncTmp).
Then, the MH filtering hook \fBslocal\fP is run
on each message.  In this mode, it is expected that
the filtering action is to put the message into
a folder.  You do this by specifying an action that
is to pipe the message into the MH rcvstore program.
See the MH man page for \fBmhook\fP for details.
There is also a patch for the slocal program that adds
a "+" syntax to maildelivery so you don't have to type in the pipeline.
.IP none
If you use an external agent to deliver messages into folders,
then you don't need Inc in the user interface.  Selecting "none"
will eliminate the Inc button altogether and disable the internal Inc procedure.
.SH MOUSE AND KEYBOARD
.PP
Several of the folder and message operations have keystroke bindings
so you can do much of your browsing with hands on the keyboard.
The default bindings from bindings.tcl are:
.DS
<Return> - Commit
<Key-i> - Inc
<Key-f> - Forward
<Key-r> - Reply
<Key-c> - Compose
<Key-p> - Previous
<Key-P> - Previous message, but do not display it.
<minus> - Previous message, even if it is marked for delete or move.
<Key-n> - Next
<Key-N> - Next message, but do not display it.
<Key-U> - Next unseen message, if any.
<Key-s> - Display the current message.
<Key-d> - Delete and advance
<Key-D> - Delete and advance, but do not display.
<Key-m> - Move and advance
<Key-M> - Move and advance, but do not display.
<Key-u> - Unmark
<space> - page forward the message display.
<BackSpace> - page back the message display.
<dollar> - Go to last message, but do not display.
<asciicircum> - Go to first message, but do not display.
.DE
.PP
In TK, the selection is dragged out with the left
mouse button.  The middle mouse button is used to drag the widget around
for scrolling purposes.  Because the left button sets the insert point and
also drags a selection, you have to be careful when setting the insert
point if you want to preserve an existing selection in another window.
The default selection-related bindings are:
.DS
<Button-1> - set insert point
<B1-Motion> - extend the selection (from insert point)
<B2-Motion> - scroll
<Control-y> - paste selection
<Control-w> - delete selection
.DE
.PP
If you would like to change the cut and paste keys, you can do this via
the "Bind" dialog.  This dialog also lets you choose which, if any,
mouse button is used for drag-scrolling.
.SH PREFERENCES
You can tune \fBexmh\fP through a dialog box that pops up when
you click the Preferences button.  A two-level scheme is used in
which the initial display is just a set of buttons that correspond
to different modules of the exmh implementation.  Clicking on one of
these buttons brings up a dialog that has settings for that module.
There is an overall explaination message, and more detailed information
will appear about each setting will appear if you click on
the short description of the preference item.
.PP
There are three types of options you can set through the Preferences dialog.
Choices are represented by radio-style buttons where only one button
in the set can be enabled at once.  Changes take effect immediately.
Booleans are represented by check-style buttons.  If the checkbox is
dark, then the option is turned on.  Changes takes effect immediately.
Numeric and filename settings have entry widgets in which you can
type in a new value.  Press <Return> for the change to take effect
immediately (or choose \fBSave\fP as described below).
.PP
You can cycle through all the preference dialogs by using the \fBNext\fP
button.
.PP
If you decide you like your settings, click \fBSave\fP in the main
Preferences dialog to save
them in a .exmh-defaults file in your home directory.
Click \fBReset All\fP in the main dialog to restore all the settings
to those of your last Save.  Within each module's preference dialog
there is a \fBReset\fP button that resets only those module's settings.
.PP
If you click \fBDismiss\fP in the main dialog, some preferences may have
been set for the current session, but they will not have been saved to
your ~/.exmh-defaults file.
.PP
Note that the .exmh-defaults file is an Xresources-style file.
You can manually add entries to the beginning of this file
to control fonts, colors, buttons, and a few other miscellanous settings.
The resources that you can set are described in more detail below.
Add the lines before the
comment about the rest of the file being automatically
generated and you will not lose your changes.

.SH EDITOR INTERACTION
If your .mh_profile Editor entry is something that \fBexmh\fP knows about
(currently just \fBmxedit\fP or \fBsedit\fP),
it uses that as your editor command.
Otherwise it uses the simple built-in editor \fBsedit\fP.
Details about the built-in editor are given below.
.PP
If you want to use another, non TK-based editor, you'll want to
start it up via the \fBexmh-async\fP wrapper script.  This lets
your editor run asynchronously, and it communicates with exmh
when your editor process completes.
If you use another TK-based editor, then you'll want to look at
editor.tcl to see how the mxedit interaction works.
If you would like to use \fBvi\fP, or some other program that
expects a controlling terminal, then run it under an xterm.
Here are a couple of examples.  Note that the name of the draft
message file will be appended to this command line, and
the name of the exmh interpreter will be inserted as the
first argument, so be careful if testing by hand.
Common examples of editor commands are:
.DS
sedit
mxedit
exmh-async xterm -e /usr/ucb/vi
exmh-async emacsclient
.DE
.PP SEDIT
The built-in editor, \fBsedit\fP, has a dozen or so commonly used editing
operations bound to keystrokes.  These are emacs-like, but
you can change them from within the editor through the "Bind" dialog.
In addition, the editor has a number of buttons and menu entries
used to manipulate your draft messages.
.IP Abort
This deletes the draft message and removes the editor window.
Use Save and Dismiss
if you want to save the message for later.
.IP Bind
This brings up a dialog that lets you edit the keystroke bindings.  The
default bindings are emacs-like, not the default TK bindings.
See the TK man page on the bind command for details about the syntax,
if you need to.  In TK versions 3.3 and later, "Meta" is an ok
synonym for "Mod1". To emulate emacs, any keystroke that is defined
to be <Meta-\fIx\fP> will also result in a binding for <Escape>\fIx\fP.
.IP "Dismiss"
This dismisses the editor.  If you have not saved, you get a dialog that
asks if you want to save or abort the message.  An abort deletes the
draft from the drafts folder, while a save leaves it in the drafts
forlder.  You can re-edit a saved draft later
by visiting the drafts folder.  While in the drafts folder,
the normal message Send button will change
to an EDIT button.  Clicking this
will bring up the editor on the saved draft.
.IP Help
This button brings up a window with a short explanation of the editor.
.IP "Insert @"
The menu item only
appears while you are replying to a message.
When you are replying to a message, the file named "@" is a symbolic link
to the message to which you are replying.  When this file is inserted,
it is formatted a bit, unlike the Insert File... operation.  The mail
headers are stripped off, and each line is prefixed with a short string
to distinguish the inserted lines.  You can set this prefix via
an Xresource, replPrefix. The default prefix is "> ".  If you want to
change this and still have trailing blanks, it turns out this works ok.
The resource value seems to begin with the first non-blank character, but
then extend all the way to the newline character.
.IP Sign
This appends your ~/.signature file to the draft.
.IP Sign...
If you have multiple files that match the ~/.signature* pattern,
the Sign button is replaced with a menu of possibilities.
.IP Send
When you are done with the message, click Send.  This is also bound
to the keystroke <Control-Return>.
.IP Whom
This button displays a small window with the network addresses of
the recipients of the message.  Clicking the button a second time
makes the window go away.
.IP Text...
This menu is used to add bold, italic, underline, etc. to your messages.
This results in a MIME message of type text/enriched.  The formatting
commands are supposed to be palatable to a non-MIME reader, but
you may want to take this into consideration.  If you disable the
"Format mail" setting (described below), the looks are discarded
before sending the message.
.IP More...
This menu contains a few entries for less common operations.  They are:
.IP "Keep on send"
This is a checkbox menu item that is off (un-highlighted) by default.
If selected it prevents the editor window from disappearing after you
send or abort a message.  This is useful if you'd like to compose
a few similar messges.  (I also use it to debug the enriched text formatter.)
.IP "Format mail"
This is a checkbox menu item that is enabled (highlighted) by default.
Formatting simply means to break long lines (> 80 characters).  This
is done as the file is saved, not as you type.  So, while the TK text
widget does line wrapping while you enter text, \fBsedit\fP does not
break these lines until you go to send the message.  If you insert a file
into a message that has critical formatting, you may want to disable the
Format option.
.IP "Attempt mhn"
If this is selected, your message is run through mhn in order to expand
any # directives that format a MIME mesage. See the mhn man page for details.
.IP "Insert File..."
This brings up a dialog box that lets you insert a file into the message.
.IP "Save"
This saves the editor buffer into a file in your drafts folder.
.IP "Spell..."
This runs the UNIX spell command over the draft body and puts the results
into a second pane inside the editor window.  Thats all - nothing fancy.
.IP "Find SEL"
This searches the draft for the current X selection.  The intent is that
you select mispelled words in the output of the spell command and use
this menu entry to locate them in the draft.  (The find and keybinding
mechanisms were stressed by this addition - expect a cleanup in version
1.3.  There is no reasonable way to define keystroke accellerators for
the find and still be able to change them in the Bind dialog. - dont'ask...).
.SH MH PROFILE
.PP
\fBExmh\fP uses a couple of things from your .mh_profile file, including
several entries that are new.
.PP
You control what headers are displayed in a message with a combination
of the Header-Suppress and Header-Display profile entries.
(Hidden headers are just scrolled off the top.)
Each of these profile entries is a list of patterns that are used to
match against the header.  Case is *not* significant in the patterns.
Its easiest to explain by giving the algorithm that uses these patterns.
.DS
By default, show all headers.
If a header is in the Header-Suppress list, do not show it.
If a header is in the Header-Display list, show it.
.DE
The default value for Header-Suppress is .*, which suppresses all
headers.  The default value for Header-Display is:
.DS
Header-Display: Subject To From Date Cc
.DE
.PP
The Folder-Order entry defines a sort ordering for your folder labels
in the folder display area.  Each item in the order can be the
name of a folder, or a pattern to match on the folder names.
All folder names that match the same pattern are sorted alphabetically.
Longer pattern matches have priority over shorter patterns.
The patterns use the syntax of TCL's string match function, which is
similar to that used in many shells.
* matches a sequence of any characters.
? matches any character.
For example, mine looks like:
.DS
Folder-Order: personal exmh mxedit * mail* sun m3 mach background
.DE
.PP
The other effect of Folder-Order is to define a traversal order for
visiting folders with unread mail in them.  When you do a Next and are
at the end of a folder, \fBexmh\fP will automatically change folders
to the next one in the Folder-Order that has unseen messages, if any.
.PP
The Folder-Ignore entry specifys a set of patterns for folder names
you want to ignore.  It defaults to .*, which causes \fBexmh\fP to
ignore all directories whose name begins with a period.
.PP
The Draft-Folder entry is used to know where to put messages being composed.
\fBExmh\fP will ask you if it is ok to create a Draft-Folder entry
if you do not already have one.
.PP
The ExmhShowProc entry lets you define a program that pre-filters
a message before displaying it.  Note that the Header-Suppress and
Header-Display mechanism is still used even if you have a special
show proc.
.PP
The Path entry is used to find your mail folders.  \fBExmh\fP will
abort if this entry is not there on the presumption that
you have not set yourself up to use MH properly.

.SH X RESOURCES
.PP
Not every X resource that controls exmh is exposed via the Preferences
dialog.  Here is a list of these extra resources.  In addition, each
TK widget has resources that it understands.  Consult the TK man pages
for details about its widgets.  Note that if a resource has a multiword
value, you *should not quote* the value in the resource file.
The right way to specify these in your ~/.exmh-defaults file is like:
.DS
*scrollbarSide:	left
*c_current: violet red
.DE
.PP
The "*" specification gets around quirks in the way TK names its applications;
different instances of the application have different names.
.SH RESOURCES FOR BUTTONS AND MENUS
.PP
Starting with version 1.2gamma, \fBexmh\fP uses X resources to specify its
buttons and menus on the main display.  (I still need to hack sedit.tcl...)
This is done by defining a list of button and menu names, and then for each
one defining a label and a command.  Menus have an additional list to define
the entries in the menu.  For each entry there is a l_ resource that defines
the label, a c_ resource that defines a command, and an optional t_ that
defines the type.  Valid menu entry types include command, radio, check, and
separator.  Hopefully this will be more clear with the following examples.
.PP
All the buttonlist and menulist resources have a parallel ubuttonlist and
umenulist resourse.  The idea is that you can append things to the set
of buttons or menu entries by defining the ubuttonlist and umenulist
resourses, and you don't need to duplicate the information about
pre-defined buttons and menu entries.  Of course, you can still override
the system button list and definitions if you want to.
.PP
.IP Main.buttonlist
The list of button names in the top row of buttons.
Default: quit pref bind colorkey help
.IP Main.ubuttonlist
A list of user buttons to be added to the set of system buttons.
Default: (empty)
.IP Main.menulist
The list of menu names in the top row.  Default: empty - no menus here.
.IP Main.umenulist
The list of user menu names in the top row.  Default: (empty)
.IP Main.quit.text
The label on the quit button.  Default: Quit
.IP Main.quit.command
The command for the quit button. Default: Exmh_Done
.PP
The remainder of the top buttons are defined as:
.DS
*Main.pref.text:	Preferences
*Main.pref.command:	Preferences_Dialog
*Main.bind.text:	Bind
*Main.bind.command:	Sedit_Pref
*Main.colorkey.text:	Color Codes
*Main.colorkey.command:	Help_KeyDisplay
*Main.help.text:	Help
*Main.help.command:	Help
.DE
.IP Fops.buttonlist
The names of the buttons in the folder operations.
Default: commit inc flist find new pick
.IP Fops.ubuttonlist
The names of user buttons to add to the set of folder operations.
Default: (empty)
.PP
The folder buttons are defined by the following entries:
.DS
*Fops.commit.text:	Commit
*Fops.commit.command:	Folder_Commit
*Fops.inc.text:		Inc
*Fops.inc.command:	Inc
*Fops.flist.text:	Flist
*Fops.flist.command:	Flist_FindUnseen
*Fops.find.text:	Find
*Fops.find.command:	Find_Setup
*Fops.new.text:		New
*Fops.new.command:	Folder_New
*Fops.pick.text:	Pick
*Fops.pick.command:	Pick
.DE
.IP Fops.menulist
The names of the menus in the folder operations.  Default: more
.IP Fops.umenulist
The names of user menus added to the folder operations.  Default: (empty)
.IP Fops.more.text
The label on the more menu.  Default: More...
.IP Fops.more.entrylist
The list of menu entries in the more menu.  Default: rescan pack sort find delete catchup
.PP
The remainder of the more menu is defined by the entries below.
.DS
*Fops.more.l_rescan: 	Rescan folder
*Fops.more.c_rescan:	busy Scan_FolderForce ; Msg_ShowSomething
*Fops.more.l_pack:	Pack folder
*Fops.more.c_pack:	busy Folder_Pack ; Msg_ShowSomething
*Fops.more.l_sort:	Sort folder
*Fops.more.c_sort:	busy Folder_Sort ; Msg_ShowSomething
*Fops.more.l_find:	Find all folders
*Fops.more.c_find:	busy Flist_Refresh
*Fops.more.l_delete:	Delete folder
*Fops.more.c_delete:	Folder_Delete
*Fops.more.l_catchup:	Catchup Unseen
*Fops.more.c_catchup:	Pick_MarkSeen
.DE
.IP Mops.buttonlist
This defines the buttons in the message operation set.
Default: move delete next prev comp reply replyall forward
.IP Mops.ubuttonlist
This defines the user buttons added to the message operation set.
Default: (empty)
.PP
The message operation buttons are defined as follows:
.DS
*Mops.move.text: 	Move
*Mops.move.command:	Msg_Move
*Mops.delete.text: 	Delete
*Mops.delete.command:	Msg_Remove
*Mops.next.text: 	Next
*Mops.next.command:	Ftoc_Next show
*Mops.prev.text: 	Prev
*Mops.prev.command:	Ftoc_Prev show
*Mops.comp.text: 	Send
*Mops.comp.command:	Msg_Compose Mh_CompSetup
*Mops.reply.text: 	reply
*Mops.reply.command:	Msg_Reply Mh_ReplySetup
*Mops.replyall.text: 	ReplyAll
*Mops.replyall.command:	Msg_Reply Mh_ReplyAllSetup
*Mops.forward.text: 	Forward
*Mops.forward.command:	Msg_Forward Mh_ForwSetup
.DE
.IP Mops.menulist
The list of menus in the message operation set.  Default: more
.IP Mops.umenulist
The list of user menus added to the message operation set.  Default: (empty)
.PP
The more menu is defined with the following entries:
.DS
*Mops.more.text: 	More...
*Mops.more.entrylist: 	print unmark clip unseen burst distrib
*Mops.more.l_print: 	Print
*Mops.more.c_print:	Print
*Mops.more.l_unmark: 	Unmark
*Mops.more.c_unmark:	Ftoc_Unmark
*Mops.more.l_clip: 	Clip
*Mops.more.c_clip:	Msg_Clip
*Mops.more.l_unseen: 	First Unseen
*Mops.more.c_unseen:	Msg_ShowUnseen
*Mops.more.l_burst: 	Burst Digest
*Mops.more.c_burst:	Msg_BurstDigest
*Mops.more.l_distrib: 	Redistribute
*Mops.more.c_distrib:	Msg_Dist Mh_DistSetup
.DE
.SH COLOR RESOURCES
.IP c_current
The color for the current message and current folder.  Default is violet red.
.IP c_unseen
The color for unseen messages and folders that contain unseen messages.
Default is blue.
.IP c_moved
The background color for message that are marked for refile,
and the background label color for the target folder for refile.
The default is yellow.
.IP c_deleted
The background color for messages marked for delete.  Default is grey75.
.IP c_foreground
The foreground color for labels in the folder display.  Default is black.
.IP c_background
The background color for labels in the folder display.  Default is white.
.IP c_popup
The color for the popups that display nested folders.  Default is wheat.
.IP c_label
The color for labels.  Three are used, for the exmh version,
the folder information, and the message information.  Default is blue.
.IP c_st_normal
The color for normal status messages.  Default is blue.
.IP c_st_error
The color for error messages. Default is purple.
.IP c_st_warn
The color for warning messages.  Default is red.
.IP c_st_background
The color for messages from the background process.  Default is medium sea green.
CHANGE.  This used to be bgMsgColor.
.SH FOLDER DISPLAY RESOURCES
.IP fl_font
The font for the labels in the folder display. Default is "fixed".
.IP fl_xgap
The horizontal gap, in pixels, between labels in the folder display.
The default is 8.
.IP fl_ygap
The vertical gap, in pixels, between labels in the folder display.
The default is 8.
.IP fl_curbutton
The button that choses the current folder in the folder display.
The default is 1. (1=left, 2=middle, 3=right).
.IP fl_navbutton
The button that navigates nested folders in the folder display.
The default is 2. (1=left, 2=middle, 3=right).
.IP fl_tarbutton
The button that choses the target folder for refile in the folder display.
The default is 3. (1=left, 2=middle, 3=right).
.SH MISCELLANEOUS RESOURCES
.PP
Most of these are now settable via the Preferences user interface.
.IP autoCommit
This is a boolean that controls whether or not exmh will commit changes to a
folder "when needed".  This is anytime the scan display is about to be
overwritten, such as when changing folders, sorting a folder, packing
a folder, or forcibly rescanning a folder.  By default this is zero (off).
.IP nextGuard
This is a boolean that controls whether or not you automatically change
to the next folder with unseen messages.  By default it is zero (no guard),
and exmh will change folders in response to a Next command if you are at
the end and there is another folder with unread messages.  If you enable
the guard, however, your first Next will give you a warning about which folder
you are about to visit, and the next Next will actually go there.
.IP impliedDirection
This is a boolean that controls whether or not the current direction
(Next or Previous) is remembered.  By default this is enabled.  This causes
exmh to move in the current direction after you delete or refile a message.
The current direction is set when you hit the Next or Prev buttons.
If you disable this, exmh always goes to the Next message after a delete or refile.
.IP replPrefix
This defines the string that is inserted before each line of a file
included with the "Insert @" entry in the built-in editor.
The default is "> ".  While trailing spaces are kept, it doesn't
seem possible to define a prefix with leading spaces;
if you use quotes they'll show up!
.IP scrollbarSide
This chooses which side the scrollbars appear on, "left" or "right".
The default is "right"
.IP m_tagnames
This defines a list of mail headers for which you want to define
special looks when they are displayed in the message area.
For each tagname, you should define another resource with name
m_tagname (e.g, m_subject) that has the text tag configuration options
for that header line.  Consult the TK man page on the text widget
to see what sort of tag configuration options there are.
In addition, two special tagnames are used for defaults if there
are no more specific matching tag name.  "general" applies to
displayed headers, while "hidden" applies to hidden ones (scrolled off the top).
The default is just "general".
.IP m_general
This is the default tagname for header lines.
The default value is {-relief flat}.  Do not put braces or quotes around
the value in your Xresource file.  As an example, here is what I
have in my own ~/.exmh-defaults file:
.DS
*m_tagnames:	hidden subject from x-filters-matched
*m_hidden:	-font 6x10
*m_general:	-foreground blue
*m_subject:	-foreground blue
*m_x-filters-matched:	-foreground blue
*m_from:	-foreground "medium sea green"
.DE
.SH WIDGET CLASS HIERARCHY
.PP
If you want to dive into the widget tree and fiddle with fonts and colors
and such, here are the class descriptions.  I also highly recommend
the tkinspect program.
.DS
.IP Main
Top row of buttons and title label
.IP Fdisp
Folder label display
.IP Fltop
Folder label display when it is in a detached toplevel.
.IP Fops
Folder operation buttons and folder label
.IP Ftoc
Folder table of contents display
.IP Mid
Frame around Face, Msgid, Status, Mops
.IP Mid.Face
Bitmap display
.IP Mid.Right.Status.label
Message label
.IP Mid.Right.Status.msg
Status line
.IP Mid.Right.Mops
Message buttons
.IP Msg
Message display
.IP Clip
Detached message display
.IP Sedit
Simple editor top-levels
.IP Help
Help window
.IP Key
Color key window
.IP Pref
Preferences dialogs
.IP Log
Error/debug log
.IP Pick
The pick dialog
.IP NewFolder
The new folder dialog.
.IP DeleteFolder
The delete folder dialog.
.IP WhatNow
The What Now? dialog.
.IP Error
Error popups
.IP Dialog
General popups
.DE
.SH GEOMETRY AND POSITION RESOURCES
.PP
Exmh now automatically remembers the position of top-level windows,
both within a session and between successive runs of exmh.
This eliminates the need to tweak the *position resources.
In addition, exmh will remember the geometry of the main window
and of the detached folder display.
However, you still may want to adjust the width and height of
some of the text windows.
Here are the default values.  
.DS
.IP exmh.iconposition: 25
(empty - no default position)
.IP exmh.iconic: 25
0
.IP *Fltop.iconposition: 25
(empty - no default position)
.IP *Fltop.iconic: 25
0
.IP *Fltop*Canvas.width: 25
300
.IP *Fltop*Canvas.height: 25
100
.IP *Sedit*Text.width: 25
80
.IP *Sedit*Text.height: 25
24
.IP *Clip*Text.width: 25
80
.IP *Clip*Text.height: 25
48
.IP *Help*Text.width: 25
80
.IP *Help*Text.height: 25
30
.IP *Log*Text.width: 25
80
.IP *Log*Text.height: 25
20
.DE

.SH INSTALLATION
Installation is done via the exmh.install script.  Invoke it as:
.DS
    wish -f ./exmh.install
.DE
This puts up a dialog box that lets you patch various file system
specific pathnames and enable or disable features that your
system cannot support (e.g., facesaver, sound).
Read the info in the dialog box and enter the appropriate file system
pathnames.
Click \fBPatch\fP to sed the exmh.MASTER script and create
the main exmh file, then click \fBTclIndex\fP to generate the library index,
and click \fBTest\fP to try it out.
If you are satisfied,
click \fBInstall\fP to copy the main script and the supporting
libraries into their destination directories.
.PP
Note: the "Script Library" and the "Install lib directory"
are usually the same place.  The script library is the run-time value
of the library directory, while the install lib directory is the
install-time value.  During the Test run, the Script
library is automatically set to "./lib" so you can run without installing.
When you hit
the Patch button, some consistency checks are done and a relative
pathname will trigger a warning.  You can ignore this when you are
testing, but you should install a working copy that only references
absolute pathnames.
.PP
(The original motivation for install-directory vs script-directory was
AFS installations in which writeable volumes have different names than
their read-only, replicated volumes.  In this case you want to install
to the writeable volume, but run from the read-only replicas...)
.PP
If you get the Script library wrong, it will be manifest as
a TCL error about an undefined procedure named Exmh.
.PP
Upon startup, the install will look for ../*/.exmhinstall so you
can retrieve configuration information from previous versions.
You can also maintain configuration information via the "Conf" button,
which looks for ../*/.exmhinstall*.
.PP
A patch to the MH slocal program is included as "misc/slocal.patch".
The patch adds a "+" syntax to your .maildelivery file that
results in messages being filed into a folder by means
of the MH \fBrcvstore\fP library program.  You can keep your patched copy of
slocal distinct from the installed version because \fBexmh\fP
runs slocal directly.  The installation dialog lets you set
up the pathname for your custom copy.
.PP
There is also a patch for the MH folder.c program.
The patch makes "folders -recurse" run much faster because
it avoids directories that do not have subfolders.
The patches apply to MH 6.7.

.SH TWEAKING EXMH
.PP
There are three levels of control over \fBexmh\fP functionality.
The first is Preferences, the second is via other Xresources
not visible via the Preferences dialog,
and the third is by overriding parts of the TCL implementation.
For TCL hacking you'll need to know where the exmh library
is so you can check out the app-defaults file and the TCL implementation.
Ask the person that installed \fBexmh\fP where the exmh library is,
or just scan the main exmh script for the definition.
.PP
Each users' personal settings are kept in a ~/.exmh-defaults file,
which is an Xresource-style file.  The section at the end is
generated by Save from the Preferences dialog.  You can add
new entries in the beginning of the file to control fonts
and colors and such.  Double-check the lib/app-defaults file
for the truth about the "hidden" resources you can play with.
.PP
Users can provide their own library of TCL scripts that augment or
override parts of the exmh implementation.  One file in particular,
user.tcl, is specifically designed to be overridden.
Another one to hack is bindings.tcl, which defines
what the keystroke bindings
are.  Start by copying either of these files into your ~/.tk/exmh
directory. (You can change this directory name via Preferences).
.PP
Remember to create a tclIndex file after update your library so the
library facility can pick up your routines.  To do this, run wish
or tclsh and utter:
.DS
auto_mkindex ~/.tk/exmh *.tcl
.DE
.PP
NOTE: if you use TCLX, then the auto_path stuff is different and the
personal library seems not to work.  If you figure out the right
thing to do in auto_path_update (in the main exmh script), let me know.
.PP
The Reply, Forward, Compose, Delete, and Move
implementations use hooks that let you override their default behavior.
For example, Msg_Reply invokes a setup procedure that composes the
initial draft of the reply, and then handles the interaction with
the editor.  It uses the hook to do the initial composition.
The default hook is Mh_SetupReply, so take a look at that, modify it,
and then you can create a button that invokes Msg_Reply with your
own hook.  Looking at the button definition in app-defaults,
and the Msg_Reply (msg.tcl) and Mh_SetupReply (mh.tcl) procedures
will help you understand what's going on.  Similarly, look at
Msg_Delete, Msg_Move, and Msg_Commit to see how they use hooks.
.PP
In addition, the user.tcl file contains two empty hook procedures,
User_Init and User_Layout.  User_Init is called early, before most
other modules are initialized.  User_Layout is called late, just
after the widget tree has been created and basically every module
initialized.  You should be able to override individual TCL procs
in User_Layout without having them trashed by the auto load facility.
In this case you'd just source a file that has the new definitions.
It turns out that TCL procs have a global scope, so you could also
just define the procs right inside User_Layout.  Say, for example, you
hate the constrained text scrolling.  Try this:
.DS
proc User_Layout {} {
    proc WidgetTextYview { t args } {
	eval {$t yview} $args
    }
}
.DE
.PP
I've tried to split up \fBexmh\fP into meaningful modules,
separating out display modules (e.g., fdisp) from those that
maintain display-independent data structures (e.g., flist).
Things like the Find and Pick dialogs are in their own file,
so you can easily replace those.
I have not documented the interfaces between modules at all,
so you'll have to read some code.
Note that the .tcl file names
reflect the names of the procedures defined in them so you
can locate definitions.  In addition, many modules use a single
global array to hold their state variables, and this array variable
has the same name as the module.

.SH BACKGROUND PROCESSING
.PP
A second process with its own interpreter
context is used to do periodic processing.  This is the \fBexmh-bg\fP
script, and it communicates with \fBexmh\fP via TK send.
With TK 3.3, send now uses xauthority mechanisms by default, unless
you compile TK with -DNO_SECURITY.  A manifestation of problems
with send are
that background inc'ing appears not to work.  If you cannot recompile
wish, then the trick is to get your X server process started with
the right incantation.  This varies from server to server, and I
do not claim to know all the details.  Again,
its all in starting up your X server properly 
(or compiling TK with -DNO_SECURITY).
.PP
The two scripts, \fBexmh\fP and \fBexmh-bg\fP,
share library procedures, so there are some little hacks
to invoke routines in the front-end interpreter, the one with the
display.  The affected routines are the Inc variations and the
Flist routines that paw through folders.  The idea is that these
routines do their file system operations in the background interpreter,
and then use the BgRPC routine to invoke display-related procedures
in the front-end interpreter that has the display.
However, in order to make it possible to invoke the Inc and Flist
routines inside the front end, BgRPC will do a local call if
it detects that it is already in the front-end. A bit hacky, but
it seems to work ok.

.SH EXAMPLES
.PP
Your ~/.maildelivery file contains the filtering specifications used by MH.
In practice, I use a small subset of the maildelivery features.
See the MH man page on \fBmhook\fP for all the details.
A .maildelivery file that goes with \fBmultidrop\fP inc'ing contains
a number of entries that concatinate matching messages onto a file:
.DS
#field	value		action	result	command
#-----	-----		------	------	-------
default	-		pipe	R	"popup -m 'New mail from $(reply-to)' -dl 1&"
Subject TAX		>	?	Mail/in.tax
Subject mxedit		>	?	Mail/in.mxedit
X-Filters-Matched mxedit >	?	Mail/in.mxedit
to	welch		>	?	Mail/in.personal
X-Filters-Matched ouster >	?	Mail/in.tcl
To	tclusers		>	?	Mail/in.tcl
cc	tclusers		>	?	Mail/in.tcl
subject	" tcl"		>	?	Mail/in.tcl
default -			>	?	Mail/in.inbox
.DE
.PP
The very last entry labeled "default" is the very most important entry in
the whole file.  Without a good default, the maildelivery system seems
happy to discard arriving messages that do not match other entries.
The '>' command says concatinate the message onto the named file.
The '?' result says, consider the message delivered if it matches,
and don't try to match a message that is already considered delivered.
Thus, the ordering of the '?' lines is significant.
The 'R' result says to perform the action and do not ever consider the
message delivered.
.PP
If you use such a .maildelivery file, then you also need a .xmhcheck file
that specifies the correspondence between inbox files and folders.
The multidrop style of inc'ing depends on such a file.
For example:
.DS
personal		Mail/in.personal
mxedit		Mail/in.mxedit
tcl		Mail/in.tcl
tax		Mail/in.tax
inbox		Mail/in.inbox
.DE
.PP
You can also pipe messages into programs.  A very useful program is the MH
recvstore program that will put an arriving message directly into a folder.
The only drag is that rcvstore is in the MH lib directory, so it isn't on
your PATH.  You have to type in the complete pathname, or create a symlink
to the program and specify that in your pipeline.  For example:
.DS
subject	"Sun-Spots Digest"	| A ".mhto  +sunspots -create"
to	"sunflash"		| A ".mhto  +sunspots -create"
to	"ravenscroft"		| A ".mhto  +dl/ravens -create"
to	"TodayatXerox"		| A ".mhto  +today -create"
to	"Modula3"		| A ".mhto  +m3 -create"
to	"nceg"			| A ".mhto  +nceg -create"
to	"WRC_Systems_Reports"	| A ".mhto  +tech -create"
to	"pencomputing"		| A ".mhto  +pencompute -create"
default	- 			| A ".mhto"
.DE
.PP
Here, the ".mhto" program is merely a symlink (in your home directory)
to the MH rcvstore program (in the MH lib directory).
The '|' says pipe the message into the program.
The 'A' result says perform the action upon a match,
regardless to whether the message is considered delivered already.
On a match, consider the message delivered.
So, the difference between '?' and 'A' actions is that multiple 'A' entries
might match on a given message, while only one '?' will.
.PP
Finally, \fBexmh\fP comes with a patch to the MH slocal program that
simplifies the process of piping a message into rcvstore.  It adds
the '+' command.  For example: (I switched from A to ? for no particular reason.)
.DS
subject           "Sun-Spots Digest"	+ ?  sunspots
to                "sunflash"		+ ?  sunspots
to                "ravenscroft"		+ ?  dl/ravens
to                "TodayatXerox"	+ ?  today
to                "Modula3"		+ ?  m3
to                "nceg"		+ ?  nceg
to                "WRC_Systems_Reports"		+ ?  tech
to                "pencomputing"	+ ?  pencompute
default           -			+ ?  inbox
.DE
.PP
Again, remember that the default entry is very important!
.SH BUGS
.PP
You cannot pin folder labels into the folder cache window.
.PP
Not all Xresources that control exmh apearance are exposed
in the Preferences dialog.

.SH AVAILABILITY
.PP
Exmh is available via FTP on parcftp.xerox.com in the pub/exmh directory.
.PP
MH is available as pub/mh/mh-6.8.tar.Z from ics.uci.edu
.PP
TCL and TK are available in pub/tcl/sprite-mirror on harbor.ecn.purdue.edu
.PP
Expect is available as pub/expect/expect.tar.Z from ftp.cme.nist.gov
.PP
Metamail (for MIME support) is available in thumper.bellcore.com:pub/nsb
.PP
Faces are available from cs.indiana.edu:/pub/faces.  The individual
databases are:
.DS
facedir.tar.Z		accumulated personal faces
facesaver.tar.Z		Usenix FaceSaver faces
logos.tar.Z		Internet domain logos
news.tar.Z		Usenet news hierarchy icons
			  (not intended for e-mail)
.DE

.SH AUTHOR
Brent Welch - welch@parc.xerox.com - suggestions welcome.
