#
proc putProc {desc name list} {
    foreach l $list {
	puts $desc "$name {$l}"
    }
}
#
proc saverc {} {
    global zircon
    set rc [glob ~]/.zirconrc
    if [file exist $rc] {
	file stat $rc st
	set mode $st(mode)
	exec mv ${rc} ${rc}.bak
    } {
	set mode 0600
    }
    set desc [open $rc w $mode]
    puts $desc "#\n# Zircon rc file saved - [exec date]\n#"
    global cVars confData DEBUG OnCode bindings trust style \
      zircon nicks ircnames ignores
    foreach z {envnick envname envserver style language command} {
	puts $desc "set zircon($z) $zircon($z)"
    }
    puts $desc "#\n# Nicknames\n#"
    putProc $desc nick $nicks
    puts $desc "#\n# IRC Names\n#"
    putProc $desc ircname $ircnames
    puts $desc "#\n# Server information\n#"
    Server :: save $desc
    puts $desc "Server :: select [$zircon(host) name]"
    puts $desc "#\n# Service information\n#"
    Service :: save $desc
    puts $desc "#\n# User information\n#"
    User :: save $desc
    puts $desc "#\n# Ignores\n#"
    foreach p $ignores {
	puts $desc "ignore {[lindex $p 0]} [lindex $p 1]"
    }
    puts $desc "#\n# Channel information\n#"
    Channel :: save $desc
    puts $desc "#\n# Message information\n#"
    Message :: save $desc
    puts $desc "#\n# Notice information\n#"
    Notice :: save $desc
    puts $desc "#\n# Chat information\n#"
    Chat :: save $desc
    puts $desc "#\n# Miscellaneous Control Values\n#"
    foreach x [array names cVars] {
	foreach v $cVars($x) {
	    global $v
	    switch $v {
	    ignores -
	    ircnames -
	    nicks { }
	    actions  { putProc $desc action $actions }
	    aways  { putProc $desc away $aways }
	    default
		{
		    if {[lsearch $confData(single) $v] >= 0} {
			puts $desc "set $v {[set $v]}"
		    } {
			puts $desc "set $v {"
			foreach x [set $v] { puts $desc "    {$x}" }
			puts $desc "}"
		    }
		}
	    }
	}
    }
    if $DEBUG { puts $desc "set DEBUG 1" }
    puts $desc "#\n# On Conditions\n#"
    foreach x [array names OnCode] {
	foreach on $OnCode($x) { puts $desc "on $x $on" }
    }
    if ![string match {} $bindings] {
	puts $desc "#\n# Global Bindings\n#"
	foreach on $bindings { puts $desc "zbind {} $on\n" }
    }
    puts $desc "#\n# Trust settings\n#"
    foreach x [array names trust] {
	puts $desc "set trust($x) {$trust($x)}"
    }
    close $desc
}
