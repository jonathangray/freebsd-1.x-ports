#
# Topic handling procs
#
proc keepTopic {this value} {
    if ![string match {} $value] {
	set chan [$this name]
	$this configure -topic $value
	set t [$this topics]
	if {[lsearch $t $value] < 0} {
	    lappend t $value
	    $this configure -topics [lsort $t]
	    [$this window].topic.label.menu add command \
	      -label "[prune $value 15]" \
	      -command "$this configure -topic {$value}"
	    if [$this keep] { global confChange ; set confChange 1 }	
	}
    }
}
#
proc getTopic {this} {
    set chan [$this name]
    mkEntryBox .@topic "${chan} Topic" "Enter your new topic for ${chan}:" \
      {{Topic {}}} "OK {$this configure -topic}" \
      "Keep {keepTopic $this}" {Cancel {}}
}
#
proc sendTopic {win} {
    if [normal $win] {
	[channel $win] configure -topic [$win get 1.0 end]
    }
}
#
proc irc331 {net prefix param pargs} {
    [Channel :: find [lindex $pargs 1]] setTopic {}
}
#
proc irc332 {net prefix param pargs} {
    [Channel :: find [lindex $pargs 1]] setTopic $param
}
