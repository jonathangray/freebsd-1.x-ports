#
# ListBox
# ----------------------------------------------------------------------
# Implements a listbox widget using primitive widgets as the building
# blocks.  This listbox is similar in function to the usual "listbox"
# widget, but it automatically manages its own scrollbar, showing it
# only when its list is too long to display.
#
#   PUBLIC ATTRIBUTES:
#
#     -list .......... list of items to be displayed
#     -width ......... width of displayed list in characters or "expand"
#     -height ........ height of displayed list in lines or "expand"
#
#   METHODS:
#
#     config ....... used to change public attributes
#     get .......... returns the current list
#
#   X11 OPTION DATABASE ATTRIBUTES
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
#      RCS:  ListBox.tcl,v 1.3 1994/04/08 13:39:19 mmc Exp
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

itcl_class ListBox {
	# ------------------------------------------------------------------
	#  CONSTRUCTOR - create new listbox
	# ------------------------------------------------------------------
	constructor {config} {
		#
		#  Create a window with the same name as this object
		#
		set class [$this info class]
		::rename $this $this-tmp-
		::frame $this -class $class
		::rename $this $this-win-
		::rename $this-tmp- $this

		#
		#  Pack widgets into this window to form a listbox
		#
		scrollbar $this.sbar -relief sunken \
			-command "$this.list yview"

		text $this.list -wrap none -relief sunken -borderwidth 2 \
			-yscrollcommand "$this.sbar set" -width 30 -height 10 \
			-cursor top_left_arrow

		set normalbg [option get $this listBackground ListBox]
		if {$normalbg == ""} {set normalbg white}
		set normalfg [option get $this listForeground ListBox]
		if {$normalfg == ""} {set normalfg black}
		$this.list config -bg $normalbg -fg $normalfg

		bind $this.list <1> { }
		bind $this.list <B1-Motion> { }
		bind $this.list <Double-1> { }
		bind $this.list <Triple-1> { }
		bind $this.list <Shift-1> { }
		bind $this.list <Shift-B1-Motion> { }
		bind $this.list <Any-Key> { }

		pack append $this \
			$this.list {left expand fill} \
			$this.sbar {right filly}

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
	#  METHOD:  get - returns the current list
	# ------------------------------------------------------------------
	method get {} {
		return $list
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
			set list [lsort $list]
			_resetArray tags
			_resetArray items

			$this.list delete 0.0 end
			foreach item $list {
				$this.list insert current "$item\n"
			}
			set line 0
			foreach item $list {
				set tag e[incr line]
				$this.list tag add $tag $line.0 [expr $line+1].0
				$this.list tag config $tag -relief flat \
					-background $normalbg -foreground $normalfg
				set tags($item) $tag
				set items($tag) $item
			}

			set htInfo [$this.list config -height]
			set ht [lindex $htInfo 4]
			if {$ht < [llength $list]} {
				pack append $this $this.sbar {right filly}
			} else {
				catch "pack unpack $this.sbar"
			}
		}
	}
	public width 30 {
		if {[winfo exists $this]} {
			if {$width == "expand"} {
				$this.list config -width {}
				pack append $this \
					$this.list {left expand fill}
			} else {
				$this.list config -width $width
				pack append $this $this.list left
			}
		}
	}
	public height 10  {
		if {[winfo exists $this]} {
			if {$height == "expand"} {
				$this.list config -height {}
				pack append $this \
					$this.list {left expand fill}
			} else {
				$this.list config -height $height
				pack append $this $this.list left
			}
		}
	}

	#
	#  PROTECTED DATA
	#    tags .......... map of list items to text widget tags
	#    items ......... map of text widget tags to list items
	#    normalbg ...... normal entry background color (without highlight)
	#    normalfg ...... normal entry foreground color (without highlight)
	#
	protected tags
	protected items
	protected normalbg {}
	protected normalfg {}
}
