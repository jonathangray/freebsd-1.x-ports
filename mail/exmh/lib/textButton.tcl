#
# $Id: textButton.tcl,v 1.1 1994/05/04 15:04:39 jkh Exp $
#
# A text widget button that acts like a Button widget.
# - John Robert LoVerso
#

set tb_priv(activebackground) #eed5b7
set tb_priv(activeforeground) Black
set tb_priv(background) #ffe4c4

proc TextButton { w text cmd } {
    global tb_priv

    if ![info exists tb_priv(seed,$w)] {
	set tb_priv(seed,$w) 0
    }

    $w insert insert {   }
    set start [$w index insert]
    $w insert insert $text
    set end [$w index insert]
    set id but[incr tb_priv(seed,$w)]

    $w tag add $id $start $end
    $w tag bind $id <Any-Enter> [concat TextButtonEnter $w $id]
    $w tag bind $id <Any-Leave> [concat TextButtonLeave $w $id]
    $w tag bind $id <1> [concat TextButtonDown $w $id]
    $w tag bind $id <ButtonRelease-1> [concat TextButtonUp $w $id [list $cmd]]
    $w tag bind $id <Any-ButtonRelease-1> [concat TextButtonUp $w $id]
    $w tag configure $id -relief raised -borderwidth 2 \
			 -background $tb_priv(background)

    $w insert insert \n
    set start $end
    $w insert insert \n
    set end [$w index insert]
    $w tag remove $id $start $end
}

#
#
# from button.tcl --
#

# The procedure below is invoked when the mouse pointer enters a
# button widget.  It records the button we're in and changes the
# state of the button to active unless the button is disabled.

proc TextButtonEnter {w id} {
    global tb_priv
    #puts "Enter"
    $w tag configure $id -background $tb_priv(activebackground)
    $w configure -cursor cross
    set tb_priv(window) $w
    set tb_priv(id) $id
}

# The procedure below is invoked when the mouse pointer leaves a
# button widget.  It changes the state of the button back to
# inactive.

proc TextButtonLeave {w id} {
    global tb_priv
    #puts "Leave"
    $w tag configure $id -background $tb_priv(background)
    $w configure -cursor xterm
    set tb_priv(window) ""
    set tb_priv(id) ""
    set tb_priv(cmd) ""
}

# The procedure below is invoked when the mouse button is pressed in
# a button/radiobutton/checkbutton widget.  It records information
# (a) to indicate that the mouse is in the button, and
# (b) to save the button's relief so it can be restored later.

proc TextButtonDown {w id} {
    global tb_priv
    #puts "Down"
    set tb_priv(relief) [lindex [$w tag config $id -relief] 4]
    set tb_priv(buttonWindow) $w
    set tb_priv(buttonId) $id
    $w tag configure $id -relief sunken
}

# The procedure below is invoked when the mouse button is released
# for a button/radiobutton/checkbutton widget.  It restores the
# button's relief and invokes the command as long as the mouse
# hasn't left the button.

proc TextButtonUp {w id {cmd {}}} {
    global tb_priv
    #puts "Up"
    if {$w == $tb_priv(buttonWindow) && $id == $tb_priv(buttonId)} {
	$w tag config $id -relief $tb_priv(relief)
	if {$w == $tb_priv(window) && $id == $tb_priv(id)} {
	    set tb_priv(cmd) $cmd
	    #puts "Primed"
	    after 1 TextButtonActivate $w $id
	}
	set tk_priv(buttonWindow) ""
	set tk_priv(buttonId) ""
    }
}
proc TextButtonActivate {w id} {
    global tb_priv
    #puts "Activate cmd=$tb_priv(cmd)"
    eval $tb_priv(cmd)
}
