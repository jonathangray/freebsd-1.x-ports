#
# FilteredBox
# ----------------------------------------------------------------------
# Implements a filtered selection box widget using primitive widgets as
# the building blocks.  A filtered selection box displays a list of
# items along with a filter entry that can be used to restrict attention
# to the items matching a string pattern.  This class merely wraps the
# SelectBox together with a few extra widgets for filtering.
#
#   PUBLIC ATTRIBUTES:
#
#     -filter ........ filter string
#     -title ......... title string displayed above selectbox
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
#     get .......... returns "all", "selected" or "showing" list
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
#   AUTHOR:  Michael J. McLennan       Phone: (610)712-2842
#            AT&T Bell Laboratories   E-mail: michael.mclennan@att.com
#
#      RCS:  FilteredBox.tcl,v 1.3 1994/04/08 13:39:16 mmc Exp
# ----------------------------------------------------------------------
#               Copyright (c) 1993  AT&T Bell Laboratories
# ======================================================================
# Permission to use, copy, modify, and distribute this software and its
# documentation for any purpose and without fee is hereby granted,
# provided that the above copyright notice appear in all copies and that
# both that the copyright notice and warranty disclaimer appear in
# supporting documentation, and that the names of AT&T Bell Laboratories
# any of their entities not be used in advertising or publicity
# pertaining to distribution of the software without specific, written
# prior permission.
#
# AT&T disclaims all warranties with regard to this software, including
# all implied warranties of merchantability and fitness.  In no event
# shall AT&T be liable for any special, indirect or consequential
# damages or any damages whatsoever resulting from loss of use, data or
# profits, whether in an action of contract, negligence or other
# tortuous action, arising out of or in connection with the use or
# performance of this software.
# ======================================================================

itcl_class FilteredBox {
	# ------------------------------------------------------------------
	#  CONSTRUCTOR - create new filtered selectbox
	# ------------------------------------------------------------------
	constructor {config} {
		#
		#  Create a window with the same name as this object
		#
		set class [$this info class]
		::rename $this $this-tmp-
		::frame $this -class $class -relief flat -borderwidth 8
		::rename $this $this-win-
		::rename $this-tmp- $this

		frame $this.s -relief sunken -borderwidth 1
		frame $this.s.r -relief raised -borderwidth 1
		pack append $this.s $this.s.r {top expand fill}

		SelectBox $this.s.r.list
		$this.s.r.list-win- config -borderwidth 4

		label $this.s.r.title -anchor s

		frame $this.s.r.filter
		label $this.s.r.filter.padR -text { }
		checkbutton $this.s.r.filter.switch -text "Filter:" \
			-relief flat -variable $this-fstate
		$this.s.r.filter.switch deselect

		entry $this.s.r.filter.e -borderwidth 2 -relief sunken
		$this.s.r.filter.e insert @0 $filter
		bind $this.s.r.filter.e <Any-Key> \
			"[bind Entry <Any-Key>]; \
			 set $this-fstate 0; $this _fstate"
		foreach oldbind [bind Entry] {
			bind $this.s.r.filter.e $oldbind "[bind Entry $oldbind]"
		}
		bind $this.s.r.filter.e <Key-Delete> \
			"[bind Entry <Key-Delete>]; \
			 set $this-fstate 0; $this _fstate"
		bind $this.s.r.filter.e <Key-Return> \
			"[bind Entry <Key-Return>]; \
			 set $this-fstate 1; $this _fstate"

		$this.s.r.filter.switch config -command "$this _fstate"
		pack append $this.s.r.filter \
			$this.s.r.filter.switch {left padx 8} \
			$this.s.r.filter.e {left expand fillx} \
			$this.s.r.filter.padR {right padx 2}

		pack append $this.s.r \
			$this.s.r.title {top fillx} \
			$this.s.r.list {top expand fill} \
			$this.s.r.filter {top pady 5 fillx}

		pack append $this $this.s {top expand fill}

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
		$this.s.r.list delete
		::rename $this-win- {}
		destroy $this
	}

	# ------------------------------------------------------------------
	#  METHOD:  get - returns "all", "selected" or "showing" lists
	# ------------------------------------------------------------------
	method get {{what all}} {
		switch $what {
			all {
				return $list
			}
			selected {
				return [$this.s.r.list get selected]
			}
			showing {
				return [$this.s.r.list get all]
			}
			default {
				error "invalid arg \"$what\": should be all, selected or showing"
			}
		}
	}

	# ------------------------------------------------------------------
	#  METHOD:  select - public access for highlighting entries
	#   USAGE:  select all
	#           select reset
	#           select entry label state
	# ------------------------------------------------------------------
	method select {args} {
		eval $this.s.r.list select $args
	}

	# ------------------------------------------------------------------
	#  METHOD:  _fstate - possible change in filter state
	# ------------------------------------------------------------------
	method _fstate {} {
		global $this-fstate

		if {$oldfstate != [set $this-fstate]} {
			_redisplay
		}
	}

	# ------------------------------------------------------------------
	#  METHOD:  _redisplay - fill listbox with current list
	# ------------------------------------------------------------------
	method _redisplay {} {
		global $this-fstate

		set alist [$this.s.r.list get selected]

		set newlist {}
		set flist {}
		if {[set $this-fstate]} {
			foreach fstr [split [$this.s.r.filter.e get] |] {
				lappend flist [string trim $fstr]
			}
		}
		if {$flist != ""} {
			foreach elem $list {
				foreach fstr $flist {
					if {[string match $fstr $elem]} {
						lappend newlist $elem
						break
					}
				}
			}
			foreach elem $alist {
				if {[lsearch $newlist $elem] < 0} {
					lappend newlist $elem
				}
			}
		} else {
			set newlist $list
		}
		$this.s.r.list config -list $newlist

		foreach entry $alist {
			$this.s.r.list select entry $entry on
		}
		set oldfstate [set $this-fstate]
	}

	#
	#  PUBLIC DATA: FilteredBox-specific options
	#
	public title "" {
		if {[winfo exists $this]} {
			if {$title != ""} {
				$this.s.r.title config -text " $title: "
			} else {
				$this.s.r.title config -text ""
			}
		}
	}
	public filter "*" {
		if {[winfo exists $this]} {
			$this.s.r.filter.e delete @0 end
			$this.s.r.filter.e insert @0 $filter
			_redisplay
		}
	}
	public list {} {
		if {[winfo exists $this]} {
			set list [lsort $list]
			$this.s.r.list config -list $list
			_redisplay
		}
	}

	#
	#  PUBLIC DATA: SelectBox options
	#
	public mode multi {
		if {[winfo exists $this]} {
			$this.s.r.list config -mode $mode
		}
	}
	public action {} {
		if {[winfo exists $this]} {
			$this.s.r.list config -action $action
		}
	}
	public width 30 {
		if {[winfo exists $this]} {
			$this.s.r.list config -width $width
		}
	}
	public height 10 {
		if {[winfo exists $this]} {
			$this.s.r.list config -height $height
		}
	}

	#
	#  PROTECTED DATA
	#    oldfstate ....... value of global $this-fstate at last display
	#                      (non-zero => filter list)
	#
	protected oldfstate 1
}
