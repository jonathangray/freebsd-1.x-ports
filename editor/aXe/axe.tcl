proc with-selection {func {args {}}} {
    set selection [get-selection]
    set start [lindex $selection 0]
    set end   [lindex $selection 1]
    set lines [lindex $selection 2]

    if {$start == $end} {return}

    set-position $start
    for {set line 0} {$line < $lines} {incr line} {
	action beginning-of-line
	eval $func $args
	action next-line
    }
    action beginning-of-line
    set-selection $start [get-position]
}

proc prefix {{text {> }}} {
    with-selection action insert-string $text
}

proc stripn {{n 1}} {
    action multiply $n
    action delete-next-character
}

proc strip {{n 1}} {
    with-selection stripn $n
}

proc join-lines {{n 1} {insert " "}} {
    for {set line 0} {$line < $n} {incr line} {
	action end-of-line
	action delete-next-character
	action insert-string $insert
    }
}

proc format-all {} {
    set caret [get-position]
    action beginning-of-file
    for {set lastpos -1; set curpos 0} \
            {$curpos != $lastpos} \
                {set lastpos $curpos; set curpos [get-position]} { 
	action forward-paragraph
	action form-paragraph
	action next-line
    }
    set-position $caret
    action redraw-display
}

proc upper {{conversion toupper}} {
    set selected [get-selection]
    set start [lindex $selected 0]
    set finish [lindex $selected 1]
    set selection [lindex $selected 3]
    
    action delete-selection
    action insert-string [string $conversion $selection]
    set-selection $start $finish
}

proc lower {} {
    upper tolower
}

# - # - # - # - # - # - # - Backwards Compatability - # - # - # - # - # - # - #

proc loopSel {func {args {}}} {
    eval with-selection $func $args
}

proc getSelection {} {
    get-selection
}

proc setSelection {from to} {
    set-selection $from $to
}

proc getPos {} {
    get-position
}

proc setPos {pos} {
    set-position $pos
}

# Compatability versions of join and format have not been
# implemented as they mask Tcl commands of the same name.
