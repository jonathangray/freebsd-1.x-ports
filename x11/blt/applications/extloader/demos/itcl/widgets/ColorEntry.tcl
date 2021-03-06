#
# ColorEntry
# ----------------------------------------------------------------------
# An entry widget for color values.  Drawn with a text label followed
# by a color sample and an entry widget.  Color names or values can
# be entered into the entry widget, and are automatically displayed
# in the sample.
#
#   PUBLIC ATTRIBUTES:
#
#     -action ........ Tcl command invoked whenever new value is installed
#     -borderwidth ... width of border around entire widget
#     -label ......... text for label identifying widget
#     -orient ........ orientation of label relative to entry
#     -width ......... width of entry widget in characters or "expand"
#
#   METHODS:
#
#     config ......... used to change public attributes
#     get ............ returns the current color
#     install ........ sets the current color
#
#   X11 OPTION DATABASE ATTRIBUTES
#
#     sampleWidth ........ width of color sample (in pixels)
#
#     ...and the rest of the usual widget attributes
#
# ----------------------------------------------------------------------
#   AUTHOR:  Michael J. McLennan       Phone: (215)770-2842
#            AT&T Bell Laboratories   E-mail: aluxpo!mmc@att.com
#
#     SCCS:  @(#)ColorEntry.tcl	1.4 (10/14/93)
# ----------------------------------------------------------------------
#            Copyright (c) 1993  AT&T  All Rights Reserved
# ======================================================================

itcl_class ColorEntry {
	# ------------------------------------------------------------------
	#  CONSTRUCTOR - create label/color/entry package
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
		#  Pack widgets into this window to form a color editor
		#
		label $this.l -anchor e
		if {$label != ""} {$this.l config -text "$label: "}

		frame $this.i -borderwidth 2 -relief flat
		set sw [option get $this sampleWidth $class]
		if {$sw == ""} {set sw 20}
		frame $this.s -borderwidth 2 -relief raised \
			-geometry [set sw]x5 -background black

		entry $this.e -relief sunken
		set defwidth [lindex [$this.e config -width] 4]
		bind $this.e <Key-Return> "$this install \[$this.e get\]"

		#
		#  Explicitly handle config's that may have been ignored earlier
		#
		foreach attr $config {
			config -$attr [set $attr]
		}
		_repack
	}

	# ------------------------------------------------------------------
	#  METHOD:  config - change public attributes
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
	#  METHOD:  get - get color value
	# ------------------------------------------------------------------
	method get {} {
		return [$this.e get]
	}

	# ------------------------------------------------------------------
	#  METHOD:  install - set color value
	# ------------------------------------------------------------------
	method install {cval} {
		$this.e delete @0 end
		$this.e insert @0 $cval
		if {[catch "$this.s config -background $cval"] != 0} {
			$this.s config -background black
		}
		if {$action != ""} {
			eval $action
		}
	}

	# ------------------------------------------------------------------
	#   METHOD:  _repack - repack internal widgets
	# ------------------------------------------------------------------
	method _repack {} {
		if {$width == "expand"} {
			pack append $this.i \
				$this.s {left fill} \
				$this.e {left expand fill}
		} else {
			pack append $this.i \
				$this.s {left fill} \
				$this.e left
		}
		if {$label != ""} {
			switch $orient {
				horizontal {
					$this.l config -anchor e
					pack append $this $this.l left \
						$this.i {left fillx}
				}
				vertical {
					$this.l config -anchor w
					pack append $this $this.l {top frame w} \
						$this.i {bottom fillx}
				}
			}
		} else {
			pack unpack $this.l
			pack append $this $this.i {bottom fillx}
		}
	}

	public action {}
	public borderwidth 3 {
		if {$borderwidth < 0} {set borderwidth 0}
		if {[winfo exists $this]} {
			$this-win- config -borderwidth $borderwidth
		}
	}
	public label "" {
		if {[winfo exists $this]} {
			if {$label != ""} {
				$this.l config -text "$label: "
			} else {
				$this.l config -text ""
			}
			_repack
		}
	}
	public orient "horizontal" {
		if {[winfo exists $this]} {
			if {$orient == "horizontal" || $orient == "vertical"} {
				_repack
			} else {
				error "bad orientation \"$orient\": should be horizontal or vertical"
			}
		}
	}
	public width "expand" {
		if {[winfo exists $this]} {
			if {$width == "expand"} {
				$this.e config -width $defwidth
			} else {
				$this.e config -width $width
			}
			_repack
		}
	}
	protected defwidth {}
}
