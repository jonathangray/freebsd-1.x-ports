#
# meeting.tcl --
#	Routines to extract meeting times from messages.
#	Interface to Sun's Calendar Manager (cm).
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc Meeting_Reload {} {
    source /project/tcl/src/exmh-2/lib/meeting.tcl
}
proc Meeting_Test {} {
    global meeting_test
    if [info exists meeting_test] {
	incr meeting_test
    } else {
	set meeting_test 1
    }
    global exwin
    Meeting_Select $exwin(mtext)
    return "Meeting_Test $meeting_test: [Meeting_Select $exwin(mtext)]"
}

proc Meeting_ParseSelection { string } {
    # Call with a text selection containing meeting information
    foreach line [split $string \n] {
	if [regexp -indices {^([^:]+):} $line match hdr] {
	    set key [string tolower [eval {string range $line} $hdr]]
	    set info($key) [string trim \
			[string range $line [expr [lindex $match 1]+1] end]]
    }
}

proc Meeting_Select { t } {
    set l 1
    set select 0
    set header 0
    for {set l 1} {1} {incr l} {
	if {[$t compare $l.0 > end]} {
	    return
	}
	set line [$t get $l.0 $l.end]
	if [regexp -nocase {parc forum} $line] {
	    return [MeetingSelectForum $t $l]
	}
	if {$line == {}} {
	    if {! $header} { 
		set header 1
		$t mark set header $l.0
	    }
	}
    }
}

proc MeetingSelectForum { t l } {
    set state date
    set date {}
    set title {}
    set speaker {}
    set abstract {}
    append text [$t get $l.0 $l.end]
    for {incr l} {[$t compare $l.0 < end]} {incr l} {
	if {$state == "done"} {
	    break
	}
	set line [$t get $l.0 $l.end]
	if {$line == {} } {
	    case $state {
		"date" { set state title }
		"title" { set state speaker }
		"speaker" { set state abstract }
		"abstract" { set state done }
	    }
	}
	case $state {
	    "date" { append date $line\n }
	    "title" { append title $line\n }
	    "speaker" { append speaker $line\n }
	    "abstract" { append abstract $line\n }
	}
    }
    return [MeetingSchedule $date $title $speaker $abstract]
}

proc MeetingSchedule { date title speaker abstract } {
    foreach word [split $date ,] {
	if [regexp -nocase {(january|february|march|april|may|june|july|august|september|october|november|december) ([0-9]+)} $word match m_str day] {
	    switch [string trim $m_str] {
		-regexp january { set month 1 }
		-regexp february { set month 2 }
		-regexp march { set month 3 }
		-regexp april { set month 4 }
		-regexp may { set month 5 }
		-regexp june { set month 6 }
		-regexp july { set month 7 }
		-regexp august { set month 8 }
		-regexp september { set month 9 }
		-regexp october { set month 10 }
		-regexp november { set month 11 }
		-regexp december { set month 12 }
		default set month $m_str
	    }
	    return "Date $month/$day/$year"
	    break ;# foreach
	}
    }
    return $date
}
