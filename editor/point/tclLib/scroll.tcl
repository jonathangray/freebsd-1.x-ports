bind XXX.scroll <ButtonPress-1>   "Scroll 1 XXX.list %y"
bind XXX.scroll <ButtonRelease-1> "StopScroll"
bind XXX.scroll <Leave>           "StopScroll"
bind XXX.scroll <ButtonPress-2>   "Scroll 2 XXX.list %y"
bind XXX.scroll <ButtonPress-3>   "Scroll 3 XXX.list %y"
bind XXX.scroll <ButtonRelease-3> "StopScroll"
bind XXX.scroll <B2-Motion>       "Scroll 2 XXX.list %y"

proc Scroll {how list y} {
	global ContinuousScrolling
	if $Button1ScrollsDown {
		if {$how==1} {
			set how 3
		} else {
			if {$how==3} {
				set how 1
			}
		}
	}
	if {$how==1} {
		set nearest [$list nearest $y]
		set top [$list nearest 0]
		set up [expr $top-$nearest]
		if {$up==0} { set up -1 }
		set new_top [expr $top+$up]
		if {$new_top<0} {set new_top 0}
		$list yview $new_top
###		set ContinuousScrolling 1
###		after 350 ScrollAgain $list $up
	} else {
		if {$how==2} {
			set high [winfo height $list]
			set size [$list size]
			set new_top [expr ($y*$size)/$high]
			$list yview $new_top
		} else {
			set top [$list nearest 0]
			set new_top [$list nearest $y]
			if {$top==$new_top} { incr new_top }
			$list yview $new_top
###			set ContinuousScrolling 1
###			set down [expr $new_top-$top]
###			after 350 ScrollAgain $list $down
		}
	}
}

proc ScrollAgain {list incr} {
	global ContinuousScrolling
	if !ContinuousScrolling { return }
	set top [$list nearest 0]
	set new_top [expr $top+$incr]
	if {$new_top<0} {set new_top 0}
	$list yview $new_top
	after 50 ScrollAgain $list $incr
}

proc StopScroll {} {
	global ContinuousScrolling
	set ContinuousScrolling 0
}

