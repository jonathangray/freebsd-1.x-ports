#
#  This file is part of GIC - the Graphical Interface to CVS
#
#  (c) Copyright 1992 Department of Computer Science, University of
#      Calgary, Calgary, Alberta, Canada.  All rights reserved.
#
#  Permission to use, copy, modify, and distribute this software and its
#  documentation for any purpose and without fee is hereby granted, provided
#  that the above copyright notice appears in all copies.  The University
#  of Calgary makes no representations about the suitability of this
#  software for any purpose.  It is provided "as is" without express or
#  implied warranty.
#

# Written by David Marwood

proc Quit {} {
    after 1 destroy .
}


#
# Procedure to pop up an error message.  Thanks goes to Michael
# Pilawa for this procedure.  Works great and insulates the user from
# those huge annoying error dumps.
# 
proc tkerror {message} {
    catch {destroy .d}
    toplevel .d -class Dialog
    set wintitle [concat "ERROR in" [winfo name .]]
    wm iconname .d $wintitle
    wm title    .d $wintitle
    frame .d.errframe -relief raised -border 1
    pack append .d.errframe \
	[message .d.errframe.mssg    -text $message \
        -anchor center      -justify center -width 400 \
        -background yellow2 -foreground red -relief raised ]  {top fill} \
	[button  .d.errframe.ok      -text "Press this button to continue." \
        -background yellow2 \
        -command { destroy .d } ] { top fill }
    pack append  .d  .d.errframe  {top expand fill frame center}
#    configcolor  .d.errframe.ok
    focus .d
    bind .d <Return>  { destroy .d }
}


set File $argv

if {$File == ""} {
    exit 1
}

wm title . "Diff of $File"

catch {exec cvs -n diff -l $File} Diff

if {[info exists Diff] == 0 || [string compare $Diff ""] == 0} {
    wm withdraw .
    ModalInfoBox "There is no difference between the version of $File in the repository and the checked out version."
    exit 0
}

text .mess -yscroll ".scroll set" -width 90 -wrap word
.mess insert end $Diff
.mess configure -state disabled
button .quit -text "Close" -command "Quit"
scrollbar .scroll -relief sunken -command ".mess yview"
pack append . .quit {bottom fillx} .scroll {right filly} .mess {top expand fill}
wm maxsize . 1000 1000
