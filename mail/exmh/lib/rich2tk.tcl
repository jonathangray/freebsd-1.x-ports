# rich2tk.tcl
#
# Read rich (or enriched) text and map it into the TK text widget.
# See /project/rfc/RFC1523.TXT
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc RichReset { t } {
    global rich
    set rich($t,family) times
    set rich($t,sizes) {60 80 100 120 140 180 240}
    set rich($t,size) 120
    set rich($t,weight) medium
    set rich($t,slant) r
    set rich($t,underline) 0
    set rich($t,color) [option get $t foreground {}]
    if ![info exists rich($t,stack)] {
	set rich($t,stack) {}
    }
}
proc Rich_Display { t fileIO type } {
    # Display a enriched or richtext body in a TK text widget
    global rich
    RichReset $t ;# clear looks stack
    set rich($t,stack) {}
    set rich($t,space) 0
    set rich($t,param) 0
    set rich($t,lastmark) [$t index insert]
    set rich($t,lasttag) {}
    set rich($t,lookMark) [$t index insert]
    while {1} {
	set numBytes [gets $fileIO line]
	if {$numBytes < 0} {
	    break
	}
	if {$numBytes == 0} {
	    RichNewline $t
	    continue
	}
	set rich($t,space) 0
	while {[string length $line]} {
	    set first [string first < $line]
	    if {$first >= 0} {
		if {($first > 0)} {
		    incr first -1
		    set string [string range $line 0 $first]
		    if {$rich($t,param)} {
			set rich($t,paramval) $string
		    } else {
			RichInsert $t $string
		    }
		    incr first
		}
		set line [string range $line $first end]
		if [string match <<* $line] {
		    RichInsert $t <
		    set line [string range $line 2 end]
		    continue
		}
		if [regexp -indices {<([^<>]+)>} $line match keyIx] {
		    set first [lindex $keyIx 0]
		    set last [lindex $keyIx 1]
		    set key [string tolower [string range $line $first $last]]
		    if {$first > 1} {
			incr first -2
			RichInsert $t [string range $line 0 $first]
		    }
		    incr last 2
		    set line [string range $line $last end]
		    set I [$t index insert]
		    if [string match /* $key] {
			set key [string range $key 1 end]
			#
			# Verify and pop format stack
			#
			set stack $rich($t,stack)
			if [RichTagPop $t $key] {
			    if {$rich($t,param)} {
				set rich($t,param) 0
				set len [llength $stack] ; incr len -1
				set stack [lreplace $stack $len $len =$rich($t,paramval)]
				set rich($t,stack) $stack
			    }
			    RichMimeTag $t $rich($t,start,$key) $I $key
			    RichTagLooks $t $rich($t,lookMark) $I $stack
			    set rich($t,lookMark) $I
			} else {
			    Exmh_Debug RichDisplay no start for $key
			}
		    } else {
			RichTagLooks $t $rich($t,lookMark) $I $rich($t,stack)
			set rich($t,lookMark) $I
			RichTagPush $t $key $I $type
		    }
		    continue
		}
	    }
	    RichInsert $t $line
	    set line {}
	}
	if {$rich($t,space)} {
	    RichInsert $t " "
	}
    }
    RichTagLooks $t $rich($t,lookMark) insert {}
    RichNewline $t
    unset rich($t,stack)
    unset rich($t,space)
    unset rich($t,lastmark)
    unset rich($t,lasttag)
    unset rich($t,lookMark)
}

proc RichKeyFromLine { lineName t ix } {
    #
    # Extract the key, insert the line before the key,
    # side-effect line to be the part after the key.
    #
    global rich
    upvar $lineName line
    set first [lindex $ix 0]
    set last [lindex $ix 1]
    set key [string tolower [string range $line $first $last]]
    incr first -2
    set string [string range $line 0 $first]
    RichInsert $t $string
    incr last 2
    set line [string range $line $last end]
    return $key
}
proc RichInsert { t string } {
    global rich
    if [string length $string] {
	set I [$t index insert]
	$t insert insert $string
	#
	# We have generated at least one character for this line,
	# so it will be appropriate to output a space at the end
	# so the next line will be properly spaced.
	#
	set rich($t,space) 1
	#
	# Undo tag 'bleeding' to inserted characters
	#
	if [info exists rich($t,lastLook)] {
	    $t tag remove $rich($t,lastLook) $I insert
	    unset rich($t,lastLook)
	}
    }
}

proc RichNewline { t } {
    global rich
    set I [$t index insert]
    $t insert insert \n
    set rich($t,space) 0
    if [info exists rich($t,lastLook)] {
	$t tag remove $rich($t,lastLook) $I insert
	unset rich($t,lastLook)
    }
}

proc RichTagPush { t key index type } {
    global rich
    # Called when a new tag appears.
    # May be isolated in text/richtext, but
    # always paired up with end-tag in text/enriched.
    global rich
    if {$type == "richtext"} {
	if [regexp {nl|np} $key] {
	    RichNewline $t
	    return
	}
	if ![string compare "lt" $key] {
	    RichInsert $t <
	    set rich($t,space) 0
	    return
	}
    }
    set rich($t,param) [expr ![string compare $key param]]
    set rich($t,start,$key) [$t index $index]
    lappend rich($t,stack) $key
}

proc RichTagPop {t key} {
    global rich
    # key should be on the top of stack.
    set len [llength $rich($t,stack)]
    incr len -1
    set top [lindex $rich($t,stack) $len]
    if [string match =* $top] {
	# Pop parameter value
	set rich($t,stack) [lreplace $rich($t,stack) $len $len]
	incr len -1
	set top [lindex $rich($t,stack) $len]
    }
    if {$top != $key} {
	Exmh_Debug RichTagPop - stack error
	Exmh_Debug " " Stack $rich($t,stack)
	Exmh_Debug " " Key $key
	return 0
    }
    set rich($t,stack) [lreplace $rich($t,stack) $len $len]
    return 1
}

proc RichMimeTag { t start end key } {
    global rich
    #
    # The Mime=foo tag is used when generating enriched text output.
    # They simply delimit the range to which a single look applies.
    # Used by SeditEnrichedExpand
    #
    if [info exists rich($t,lastmark)] {
	if [$t compare $start > "$rich($t,lastmark) +1 c"] {
	    #
	    # Undo the tag in gaps between commands so that
	    # formatting does not "bleed" with inserted characters
	    #
	    $t tag remove $rich($t,lasttag) "$rich($t,lastmark) +1 c" $start
	}
	set rich($t,lastmark) $end
	set rich($t,lasttag) Mime=$key
    }
    $t tag add Mime=$key $start $end
}

proc Rich_TagRange { t start end key type } {
    global rich
    #
    # Called from a composer to set looks in a text widget.
    #
    RichMimeTag $t $start $end $key
    RichReset $t
    #
    # The Looks for the range are a function of existing looks within
    # the range.  So, this new range will break up pre-existing looks
    # ranges and modify them.  The Looks tag includes the formatting
    # stack needed to compute the proper looks.
    #
    set end [$t index $end]
    set start [$t index $start]
    set rich($t,lookMark) $start
    set curStack $key
    set forwMark {}
    Exmh_Debug Rich_TagRange $start $end $key
    Exmh_Debug Stack := $curStack
    for {set ix $start} {[$t compare $ix < $end]} {set ix [$t index "$ix +1c"]} {
	foreach tag [$t tag names $ix] {
	    if [regexp {Look=(.+)} $tag match stack] {
		Exmh_Debug $tag | $rich($t,lookMark) $ix cur=$curStack new=$stack
		RichTagLooks $t $rich($t,lookMark) $ix $curStack
		set rich($t,lookMark) $ix
		set curStack $stack
		lappend curStack $key
		Exmh_Debug Stack => $curStack
		set range [$t tag nextrange $tag $ix]
		if {$range != {}} {
		    Exmh_Debug $t tag remove $tag $range
		    eval {$t tag remove $tag} $range
		    set forwMark [lindex $range 1]
		}
		continue
	    }
	    if {$ix == $forwMark} {
		# end of previously found range
		Exmh_Debug <forw> | $rich($t,lookMark) $ix $curStack
		RichTagLooks $t $rich($t,lookMark) $ix $curStack
		set rich($t,lookMark) $ix
		# Assert, only get here if no Looks ahead, so stack is just $key
		set curStack $key
		Exmh_Debug Stack => $curStack
	    }
	}
    }
    Exmh_Debug Stack := $curStack
    Exmh_Debug <end> | $rich($t,lookMark) $ix $curStack
    RichTagLooks $t $rich($t,lookMark) $end $curStack
}
proc Rich_TagClear { t start end } {
    global rich
    #
    # Called from a composer to clear looks in a text widget.
    #
    RichReset $t
    #
    # The Looks for the range are a function of existing looks within
    # the range.  So, this new range will break up pre-existing looks
    # ranges and modify them.  The Looks tag includes the formatting
    # stack needed to compute the proper looks.
    #
    set end [$t index $end]
    set start [$t index $start]
    set rich($t,lookMark) $start
    set forwMark {}
    Exmh_Debug Rich_TagClear $start $end
    for {set ix $start} {[$t compare $ix < $end]} {set ix [$t index "$ix +1c"]} {
	foreach tag [$t tag names $ix] {
	    if [regexp {Look=(.+)} $tag match stack] {
		$t tag remove $tag $ix $end
	    }
	    if [regexp {Mime=(.+)} $tag match stack] {
		$t tag remove $tag $ix $end
	    }
	}
    }
}
proc RichTagLooks { t start end stack } {
    global rich
    if {$start == $end} {
	return
    }
    RichLooksFromStack $t $stack
    set font *-$rich($t,family)-$rich($t,weight)-$rich($t,slant)-*-*-*-$rich($t,size)-*-*-*-*-*-*
    $t tag configure Look=$stack -underline $rich($t,underline)
    if [catch {
	$t tag configure Look=$stack -font $font 
    } err] {
	$t tag configure Look=$stack -font fixed \
	    -foreground black -background white
    }
    if [catch {
	$t tag configure Look=$stack -foreground $rich($t,color)
    } err] {
	$t tag configure Look=$stack -foreground black -background white
    }
    $t tag add Look=$stack $start $end
    #
    # This is used to undo look "bleeding" into subsequently inserted chars.
    #
    set rich($t,lastLook) Look=$stack
    set rich($t,lastLookEnd) $end
}
proc RichLooksFromStack { t stack } {
    global rich
    set ignore 0
    RichReset $t
    foreach look $stack {
	case $look {
	    default { incr ignore }
	    =* {
		set paramval [string range $look 1 end]
		set $needsParam $paramval
	    }
	    x-plain { RichReset $t }
	    x-color { set needsParam rich($t,color) }
	    fixed { set rich($t,family) courier }
	    underline { set rich($t,underline) 1 }
	    bold { set rich($t,weight) bold }
	    italic { set rich($t,slant) i }
	    smaller {
		set ix [lsearch $rich($t,sizes) $rich($t,size)]
		if {$ix > 0} {
		    incr ix -1
		    set rich($t,size) [lindex $rich($t,sizes) $ix]
		}
	    }
	    bigger {
		set ix [lsearch $rich($t,sizes) $rich($t,size)]
		incr ix
		if {$ix < [llength $rich($t,sizes)]} {
		    set rich($t,size) [lindex $rich($t,sizes) $ix]
		}
	    }
	}
    }
    return ignore
}

