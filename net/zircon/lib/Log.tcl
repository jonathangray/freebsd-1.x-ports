#
proc logOpen {this mode file} {
    if [string match {} $file] { return }
    global Clog Clogfile
    if [catch "open $file $mode" Clog($this)] {
	info0 addText ERROR \
	  "*** Cannot open log file for channel [$this name] : $Clog($this)"
	unset Clog($this)
	return
    }
    set Clogfile($this) $file
    set w [$this window].channel.menu.log
    $w entryconfigure 0 -state normal
    $w entryconfigure 1 -state disabled
    $w entryconfigure 2 -state normal
    $w entryconfigure 3 -state normal
}

proc channel_doLog {this op} {
    global Clog Clogfile
    set w [$this window].channel.menu.log
    switch $op {
    Close {
	    if ![string match {} Clog($this)] {
		close $Clog($this)
		set Clog($this) {}
	    }
	    $w entryconfigure 0 -state disabled
	    $w entryconfigure 1 -state normal
	    $w entryconfigure 2 -state disabled
	    $w entryconfigure 3 -state disabled
	}
    Empty {
	    if [info exists $Clog($this)] {
		close $Clog($this)
		set Clog($this) [open $Clogfile($this) w]
	    }
	}
    Flush { if [info exists Clog($this)] { flush $Clog($this) } }
    Open {
	    set chan [$this name]
	    set fl [expr {[string match "" $Clogfile($this)] ? \
	      $Clogfile($this) : "${chan}.log"}]
	    mkFileBox .@log$this "Log ${chan}" \
	      "Log file for channel ${chan}:" {} \
	      "Append {logOpen $this a }"\
	      "Truncate {logOpen $this w }" {Cancel {}}
	}
    }
}
