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

# Simply displays $Message in a new modal window (modal windows 
# prevent access to the rest of the application) with an "Okay" button
# until the button is pressed.  Returns nothing.
proc ModalInfoBox Message {
    ModalButtonBox $Message "Okay" "Information"
}


# $ButtonNames is a list of button names.  Displays $Message in a new (modal) window
# and creates a button for each member of $ButtonNames.  
# Returns the index in $ButtonNames of the button selected.
set ModalButtonBoxAns 0
proc ModalButtonBox {Message ButtonNames {Title "Question"}} {
    global ModalButtonBoxAns
    toplevel .question
    wm title .question $Title
    message .question.mess -text $Message -aspect 400
    pack append .question .question.mess top
    foreach ButtonName $ButtonNames {
	button .question.x$ButtonName -text $ButtonName -command "set ModalButtonBoxAns [lsearch $ButtonNames $ButtonName]; destroy .question"
	pack append .question .question.x$ButtonName left
    }
    grab set .question
    tkwait window .question
    return $ModalButtonBoxAns
}

# Displays $Message in a new (modal) window with an "entry" widget
# and an "Okay" and "Cancel" button below it.  
# Returns the text in the entry widget when the Okay button is
# pressed, or "-1" if the Cancel button is pressed.
set ModalTextBoxAns 0
proc ModalTextBox {Message {Title "Text entry"}} {
    global ModalTextBoxAns
    toplevel .entry
    wm title .entry $Title
    message .entry.mess -text $Message -aspect 400
    button .entry.cancel -text Cancel -command "set ModalTextBoxAns -1; destroy .entry"
    button .entry.okay -text Okay -command "set ModalTextBoxAns \[.entry.entry get\]; destroy .entry"
    entry .entry.entry -relief sunken
    pack append .entry .entry.mess top .entry.cancel right .entry.okay right .entry.entry {right fill}
    tkwait window .entry
    return $ModalTextBoxAns
}


