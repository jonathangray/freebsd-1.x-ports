#
# SelectBox
# ----------------------------------------------------------------------
# Implements a selection box widget using primitive widgets as the
# building blocks.  A selection box widget displays a list of items
# and allows the user to scroll through the list and select single
# or multiple items.  This class is derived from ListBox, and so
# it inherits the basic listbox display behavior.
#
#   PUBLIC ATTRIBUTES:
#
#     -mode .......... single/multi selection
#     -action ........ callback invoked whenever entry is selected/unselected
#
#     -list .......... list of items to be displayed
#     -width ......... width of displayed list in characters or "expand"
#     -height ........ height of displayed list in lines or "expand"
#
#   METHODS:
#
#     config ....... used to change public attributes
#     get .......... returns "all" or "selected" list
#     select ....... select/unselect entries programmatically
#
#   X11 OPTION DATABASE ATTRIBUTES
#
#     selectBackground ... background color for selected entries
#     selectForeground ... foreground color for selected entries
#
#     listBackground ..... background color for entries
#     listForeground ..... foreground color for entries
#
#     ...and the rest of the usual widget attributes
#
# ----------------------------------------------------------------------
#   AUTHOR:  Michael J. McLennan       Phone: (215)770-2842
#            AT&T Bell Laboratories   E-mail: aluxpo!mmc@att.com
#
#     SCCS:  @(#)SelectBox.tcl	1.4 (10/14/93)
# ----------------------------------------------------------------------
#            Copyright (c) 1993  AT&T  All Rights Reserved
# ======================================================================

itcl_class SelectBox {
	inherit ListBox

	# ------------------------------------------------------------------
	#  CONSTRUCTOR - create new selectbox
	# ------------------------------------------------------------------
	constructor {config} {
		ListBox::constructor

		set normalbg [option get $this listBackground SelectBox]
		if {$normalbg == ""} {set normalbg white}
		set normalfg [option get $this listForeground SelectBox]
		if {$normalfg == ""} {set normalfg black}
		$this.list config -bg $normalbg -fg $normalfg

		set selectfg [option get $this selectForeground SelectBox]
		set selectbg [option get $this selectBackground SelectBox]

		switch [tk colormodel $this] {
			monochrome {
				if {$selectbg == ""} {set selectbg black}
				if {$selectfg == ""} {set selectfg white}
			}
			color {
				if {$selectbg == ""} {set selectbg LightSteelBlue}
				if {$selectfg == ""} {set selectfg black}
			}
		}
		set focusbg [option get $this focusBackground SelectBox]
		if {$focusbg == ""} {set focusbg $normalfg}
		set focusfg [option get $this focusForeground SelectBox]
		if {$focusfg == ""} {set focusfg $normalbg}

		bind $this.list <1> "$this _clickStart \[%W index @%x,%y\]"
		bind $this.list <Double-1> { }
		bind $this.list <Triple-1> { }
		bind $this.list <Shift-1> { }
		bind $this.list <Shift-B1-Motion> { }
		bind $this.list <Any-Key> { }

		_resetArray active
		config -mode $mode

		#
		#  Explicitly handle config's that may have been ignored earlier
		#
		foreach attr $config {
			config -$attr [set $attr]
		}
	}

	# ------------------------------------------------------------------
	#  METHOD:  config - used to change public attributes
	# ------------------------------------------------------------------
	method config {config} {}

	# ------------------------------------------------------------------
	#  DESTRUCTOR - destroy window containing widget
	# ------------------------------------------------------------------
	destructor {
		::rename $this-win- {}
		destroy $this
	}

	# ------------------------------------------------------------------
	#  METHOD:  get - returns "all" or "selected" list
	# ------------------------------------------------------------------
	method get {{what all}} {
		switch $what {
			all {
				return $list
			}
			selected {
				set selns {}
				foreach tag [array names active] {
					lappend selns $active($tag)
				}
				return [lsort $selns]
			}
			default {
				error "invalid arg \"$what\": should be all or selected"
			}
		}
	}

	# ------------------------------------------------------------------
	#  METHOD:  select - public access for highlighting entries
	#   USAGE:  select all
	#           select reset
	#           select entry label state
	# ------------------------------------------------------------------
	method select {how args} {
		switch $how {
			reset {
				foreach tag [array names active] {
					_deactivate $tag
				}
			}
			all {
				foreach item $list {
					_activate $tags($item)
				}
			}
			entry {
				if {[llength $args] != 2} {
					error "improper usage: should be \"select entry label state\""
				}
				set label [lindex $args 0]
				set state [lindex $args 1]
				if {[info exists tags($label)]} {
					set tag $tags($label)
					switch $state {
						on {
							if {$mode == "single"} {
								foreach old [array names active] {
									if {$old != $tag} {
										_deactivate $old
									}
								}
							}
							_activate $tag
						}
						off {
							_deactivate $tag
						}
						default {
							error "improper state: should be \"on\" or \"off\""
						}
					}
				} else {
					error "entry not found in SelectBox $this: $label"
				}
			}
			default {
				error "wrong # args: should be \"select type ?args?\""
			}
		}
	}

	# ------------------------------------------------------------------
	#  METHOD:  _hilite - temporary highlight when pointer is over entry
	# ------------------------------------------------------------------
	method _hilite {tag state} {
		switch $state {
			on {
				$this.list tag config $tag \
					-background $focusbg -foreground $focusfg
			}
			off {
				if {[info exists active($tag)]} {
					$this.list tag config $tag -relief raised \
						-background $selectbg -foreground $selectfg
				} else {
					$this.list tag config $tag -relief flat \
						-background $normalbg -foreground $normalfg
				}
			}
		}
	}

	# ------------------------------------------------------------------
	#  METHOD:  _activate - add item to "active" list
	# ------------------------------------------------------------------
	method _activate {tag} {
		if {$tag != ""} {
			if {![info exists active($tag)]} {
				set active($tag) $items($tag)
			}
			$this.list tag config $tag -relief raised \
				-background $selectbg -foreground $selectfg

			if {$action != ""} {
				eval $action [list $items($tag)] on
			}
		}
	}

	# ------------------------------------------------------------------
	#  METHOD:  _deactivate - remove item from "active" list
	# ------------------------------------------------------------------
	method _deactivate {tag} {
		if {$tag != ""} {
			$this.list tag config $tag -relief flat \
				-background $normalbg -foreground $normalfg

			if {[info exists active($tag)]} {
				unset active($tag)

				if {$action != ""} {
					eval $action [list $items($tag)] off
				}
			}
		}
	}

	# ------------------------------------------------------------------
	#  METHOD:  _clickStart - invoked for button-press selection
	# ------------------------------------------------------------------
	method _clickStart {current} {
		$this.list mark set selstart current
		$this.list mark set sellast current
		set tag [$this.list tag names $current]

		if {$mode == "single"} {
			foreach old [array names active] {
				if {$old != $tag} {
					_deactivate $old
				}
			}
		} else {
			set snapshot [array names active]
		}

		if {[info exists active($tag)]} {
			set sweep unselect
			_deactivate $tag
		} else {
			set sweep select
			_activate $tag
		}
	}

	# ------------------------------------------------------------------
	#  METHOD:  _clickSweep - invoked for button-press movement
	# ------------------------------------------------------------------
	method _clickSweep {current} {
		set start [$this.list index selstart]
		set dist [expr abs($current-$start)]
		set last [expr abs([$this.list index sellast]-$start)]

		set op $sweep
		if {$dist < $last} {
			set op restore
		}

		set cline [lindex [split $current "."] 0]
		set lline [lindex [split [$this.list index sellast] "."] 0]
		if {$lline < $cline} {
			set inc 1
		} else {
			set inc -1
		}

		for {set line $lline} {$line != [expr $cline+$inc]} {incr line $inc} {
			set tag [$this.list tag names $line.0]
			switch $op {
				select {
					_activate $tag
				}
				unselect {
					_deactivate $tag
				}
				restore {
					if {[lsearch $snapshot $tag] >= 0} {
						_activate $tag
					} else {
						_deactivate $tag
					}
				}
			}
		}
		_hilite $current on
		$this.list mark set sellast $current
	}

	# ------------------------------------------------------------------
	#  METHOD:  _resetArray - clear array to empty state
	# ------------------------------------------------------------------
	method _resetArray {name} {
		catch "unset $name"
		set ${name}(0) "make-this-an-array"
		unset ${name}(0)
	}

	#
	#  PUBLIC DATA
	#
	public list {} {
		if {[winfo exists $this]} {
			foreach tag [array names active] {
				if {$action != ""} {
					eval $action [list $active($tag)] off
				}
			}
			ListBox::config -list $list
			_resetArray active

			foreach item [array names tags] {
				set tag $tags($item)
				$this.list tag bind $tag <Enter> "$this _hilite $tag on"
				$this.list tag bind $tag <Leave> "$this _hilite $tag off"
			}
		}
	}
	public mode multi {
		if {[winfo exists $this]} {
			set skip 1
			if {$mode == "single"} {
				foreach tag [array names active] {
					if {!$skip} {
						_deactivate $tag
					}
					set skip 0
				}
				bind $this.list <B1-Motion> { }
			} else {
				bind $this.list <B1-Motion> \
					"$this _clickSweep \[%W index @%x,%y\]"
			}
		}
	}
	public action {}

	#
	#  PROTECTED DATA
	#    active ......... array of tags for selected entries
	#    snapshot ....... snapshot of "active" list used during click-drag
	#    sweep .......... select/unselect type of click-drag sweep
	#
	#    selectbg ....... background color for selected entries
	#    selectfg ....... foreground color for selected entries
	#    focusbg ........ background color when pointer is over entry
	#    focusfg ........ foreground color when pointer is over entry
	#
	protected active
	protected snapshot {}
	protected sweep {}

	protected selectbg {}
	protected selectfg {}
	protected focusbg {}
	protected focusfg {}
}
