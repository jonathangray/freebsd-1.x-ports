#
# ColorEditor
# ----------------------------------------------------------------------
# An HSV color editor for composing color values.
#
#   PUBLIC ATTRIBUTES:
#
#     -borderwidth ... width of border around entire widget
#     -size .......... size of the HS editor window
#
#   METHODS:
#
#     config ......... used to change public attributes
#     get ............ returns the current color
#     install ........ sets the current color
#
#   X11 OPTION DATABASE ATTRIBUTES:
#
#     sampleWidth ........ width of color sample (in pixels)
#
#     ...and the rest of the usual widget attributes
#
# ----------------------------------------------------------------------
#   AUTHOR:  Michael J. McLennan       Phone: (610)712-2842
#            AT&T Bell Laboratories   E-mail: michael.mclennan@att.com
#
#      RCS:  ColorEditor.tcl,v 1.2 1994/04/08 13:39:12 mmc Exp
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

itcl_class ColorEditor {

	# ------------------------------------------------------------------
	#  CONSTRUCTOR - create color editor package
	# ------------------------------------------------------------------
	constructor {config} {
		#
		#  Create a window with the same name as this object
		#
		set class [$this info class]
		::rename $this $this-tmp-
		::frame $this -class $class -borderwidth 8
		::rename $this $this-win-
		::rename $this-tmp- $this

		#
		#  Pack widgets into this window to form a color editor
		#
		frame $this.s -borderwidth 1 -relief sunken
		pack append $this $this.s top

		frame $this.s.r -borderwidth 1 -relief raised
		pack append $this.s $this.s.r top

		set bg [lindex [$this-win- config -background] 4]
		canvas $this.s.r.valHS -background $bg -width $size -height $size
		set ht [lindex [$this.s.r.valHS config -height] 4]
		$this.s.r.valHS config -height [expr $ht*0.866]

		scale $this.s.r.valV -orient vertical -from 100 -to 0 \
			-command "$this _updateV"

		frame $this.s.r.valHSV -borderwidth $borderwidth
		lower $this.s.r.valHSV
		pack append $this.s.r.valHSV \
			$this.s.r.valHS left \
			$this.s.r.valV {right filly}

		ColorEntry $this.s.r.value -label "Color Value" \
			-action "$this install \[$this.s.r.value get\]"

		pack append $this.s.r \
			$this.s.r.value {top fillx} \
			$this.s.r.valHSV top

		#
		#  Set up bindings for H/S editor on canvas
		#
		bind $this.s.r.valHS <Configure> "$this _drawHS; $this install black"

		bind $this.s.r.valHS <ButtonPress-1> "$this _updateHS free %x %y"
		bind $this.s.r.valHS <B1-Motion> "$this _updateHS free %x %y"

		bind $this.s.r.valHS <ButtonPress-2> \
			"$this _drawGuide fixH; $this _updateHS fixH %x %y"
		bind $this.s.r.valHS <B2-Motion> "$this _updateHS fixH %x %y"
		bind $this.s.r.valHS <ButtonRelease-2> "$this _drawGuide erase"

		bind $this.s.r.valHS <ButtonPress-3> \
			"$this _drawGuide fixS; $this _updateHS fixS %x %y"
		bind $this.s.r.valHS <B3-Motion> "$this _updateHS fixS %x %y"
		bind $this.s.r.valHS <ButtonRelease-3> "$this _drawGuide erase"

		#
		#  Explicitly handle config's that may have been ignored earlier
		#
		foreach attr $config {
			config -$attr [set $attr]
		}
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
		return [$this.s.r.value get]
	}

	# ------------------------------------------------------------------
	#  METHOD:  install - set color value
	# ------------------------------------------------------------------
	method install {cval} {
		eval _set_rgb [winfo rgb $this $cval]

		_drawHSpos
		_updateValue $cval

		$this.s.r.valV config -command {concat}
		$this.s.r.valV set [lindex [split [expr $valV*100] .] 0]
		$this.s.r.valV config -command "$this _updateV"
	}

	# ------------------------------------------------------------------
	#  METHOD:  _drawHS - update HS display in new window size
	# ------------------------------------------------------------------
	method _drawHS {} {
		eval $this.s.r.valHS delete [$this.s.r.valHS find all]

		set w [winfo width $this.s.r.valHS]
		set wc [expr $w/2]
		set hc [expr [winfo height $this.s.r.valHS]/2]
		set blen [expr 0.9*($w-2*$borderwidth)/2]
		set slen [expr 0.1*($w-2*$borderwidth)/2]

		_drawCircle [expr $wc+$blen] $hc $slen {} red
		_drawCircle [expr $wc+0.5*$blen] [expr $hc-0.866*$blen] $slen {} yellow
		_drawCircle [expr $wc-0.5*$blen] [expr $hc-0.866*$blen] $slen {} green
		_drawCircle [expr $wc-$blen] $hc $slen {} cyan
		_drawCircle [expr $wc-0.5*$blen] [expr $hc+0.866*$blen] $slen {} blue
		_drawCircle [expr $wc+0.5*$blen] [expr $hc+0.866*$blen] $slen {} magenta

		_drawCircle $wc $hc $blen black white
		set HSsize $blen

		_drawHSpos
	}

	# ------------------------------------------------------------------
	#  METHOD:  _drawGuide - draw special guides for fixed H/S values
	# ------------------------------------------------------------------
	method _drawGuide {mode} {
		set wc [expr [winfo width $this.s.r.valHS]/2]
		set hc [expr [winfo height $this.s.r.valHS]/2]

		switch $mode {
			fixH {
				set xy [_hs2xy $valH 1.0]
				eval $this.s.r.valHS create line $wc $hc $xy \
					-width 3 -tags GUIDE
				eval $this.s.r.valHS create line $wc $hc $xy \
					-fill white -tags GUIDE
			}
			fixS {
				set len [expr $valS*$HSsize]
				set tags [_drawCircle $wc $hc $len white {}]
				foreach tag $tags {
					$this.s.r.valHS itemconfig $tag -tags GUIDE
				}
			}
			erase {
				$this.s.r.valHS delete GUIDE
			}
		}
	}

	# ------------------------------------------------------------------
	#  METHOD:  _updateHS - update HS values from (x,y) coordinate
	# ------------------------------------------------------------------
	method _updateHS {mode x y} {
		set hs [_xy2hs $x $y]
		switch $mode {
			free {
				set valH [lindex $hs 0]
				set valS [lindex $hs 1]
			}
			fixH {
				set valS [lindex $hs 1]
			}
			fixS {
				set valH [lindex $hs 0]
			}
		}
		_drawHSpos
		_updateValue [_hsv2color $valH $valS $valV]

		update idletasks
	}

	# ------------------------------------------------------------------
	#  METHOD:  _updateV - update V value from scale
	# ------------------------------------------------------------------
	method _updateV {val} {
		set valV [expr $val/100.0]
		_drawHSpos
		_updateValue [_hsv2color $valH $valS $valV]
	}

	# ------------------------------------------------------------------
	#  METHOD:  _updateValue - update #xxxxxx color value shown in entry
	# ------------------------------------------------------------------
	method _updateValue {cval} {
		$this.s.r.value config -action {}
		$this.s.r.value install $cval
		$this.s.r.value config \
			-action "$this install \[$this.s.r.value get\]"
	}

	# ------------------------------------------------------------------
	#  METHOD:  _drawHSpos - draw current HS position on HS display
	# ------------------------------------------------------------------
	method _drawHSpos {} {
		$this.s.r.valHS delete MARKER

		set wc [expr [winfo width $this.s.r.valHS]/2]
		set hc [expr [winfo height $this.s.r.valHS]/2]
		set len [expr $valS*$HSsize]

		set tags [_drawCircle $wc $hc $len black {}]
		foreach tag $tags {
			$this.s.r.valHS itemconfig $tag -tags MARKER -width 3
		}
		$this.s.r.valHS raise GUIDE

		set xy [_hs2xy $valH $valS]
		set x [lindex $xy 0]
		set y [lindex $xy 1]
		set color [_hsv2color $valH $valS $valV]
		set tag [
			_drawCircle $x $y [expr 0.07*$HSsize] black $color
		]
		$this.s.r.valHS addtag MARKER withtag $tag
	}

	# ------------------------------------------------------------------
	#  METHOD:  _drawCircle - draw circle into HS display
	# ------------------------------------------------------------------
	method _drawCircle {x y rad linec fillc} {
		if {$fillc != ""} {
			set fill "-fill $fillc"
		} else {
			set fill "-fill {}"
		}
		if {$linec != ""} {
			set line "-outline $linec"
		} else {
			set line "-outline {}"
		}
		return [
			eval $this.s.r.valHS create oval \
				[expr $x-$rad] [expr $y-$rad] \
				[expr $x+$rad] [expr $y+$rad] \
				$fill $line
		]
	}

	# ------------------------------------------------------------------
	#  METHOD:  _xy2hs - convert (x,y) on canvas to (hue,saturation)
	#
	#  INPUTS:  x = x-coordinate on canvas in pixels
	#           y = y-coordinate on canvas in pixels
	# ------------------------------------------------------------------
	method _xy2hs {x y} {
		set dx [expr $x-([winfo width $this.s.r.valHS]/2)]
		set dy [expr ([winfo height $this.s.r.valHS]/2)-$y]

		if {$dx == 0.0} {
			if {$dy > 0} {
				set h 1.570795
			} else {
				set h 4.712385
			}
		} else {
			set h [expr atan($dy*1.0/$dx)]
		}
		if {$dy > 0} {
			if {$dx < 0} {
				set h [expr $h+3.14159]
			}
		} else {
			if {$dx < 0} {
				set h [expr $h+3.14159]
			} else {
				set h [expr $h+6.28318]
			}
		}
		set s [expr sqrt($dx*$dx+$dy*$dy)/$HSsize]
		if {$s > 1.0} {set s 1.0}
		return [list $h $s]
	}

	# ------------------------------------------------------------------
	#  METHOD:  _hs2xy - convert (hue,saturation) to (x,y) on canvas
	#
	#  INPUTS:  h = hue angle in radians
	#           s = saturation value (0.0-1.0)
	# ------------------------------------------------------------------
	method _hs2xy {h s} {
		set wc [expr [winfo width $this.s.r.valHS]/2]
		set hc [expr [winfo height $this.s.r.valHS]/2]

		return [list \
			[expr $wc+cos($h)*$s*$HSsize] \
			[expr $hc-sin($h)*$s*$HSsize] \
		]
	}

	# ------------------------------------------------------------------
	#  METHOD:  _set_rgb - set current internal HSV values to RGB color
	# ------------------------------------------------------------------
	method _set_rgb {r g b} {
		set r [expr $r/65535.0]
		set g [expr $g/65535.0]
		set b [expr $b/65535.0]

		set max 0
		if {$r > $max} {set max $r}
		if {$g > $max} {set max $g}
		if {$b > $max} {set max $b}

		set min 65535
		if {$r < $min} {set min $r}
		if {$g < $min} {set min $g}
		if {$b < $min} {set min $b}

		set valV $max
		if {$max != 0} {
			set valS  [expr ($max-$min)/$max]
		} else {
			set valS 0
		}
		if {$valS != 0} {
			set rc [expr ($max-$r)/($max-$min)]
			set gc [expr ($max-$g)/($max-$min)]
			set bc [expr ($max-$b)/($max-$min)]

			if {$r == $max} {
				set valH [expr $bc-$gc]
			} elseif {$g == $max} {
				set valH [expr 2+$rc-$bc]
			} elseif {$b == $max} {
				set valH [expr 4+$gc-$rc]
			}
			set valH [expr $valH*1.0472]
			if {$valH < 0} {set valH [expr $valH+6.28318]}
		}
	}

	# ------------------------------------------------------------------
	#  PROC:  _hsv2color - convert color value in HSV to #xxxxxx
	# ------------------------------------------------------------------
	proc _hsv2color {h s v} {
		if {$s == 0} {
			set r $v
			set g $v
			set b $v
		} else {
			if {$h >= 6.28318} {set h [expr $h-6.28318]}
			set h [expr $h/1.0472]
			set f [expr $h-floor($h)]
			set p [expr $v*(1.0-$s)]
			set q [expr $v*(1.0-$s*$f)]
			set t [expr $v*(1.0-$s*(1.0-$f))]

			switch [lindex [split $h .] 0] {
				0 {set r $v; set g $t; set b $p}
				1 {set r $q; set g $v; set b $p}
				2 {set r $p; set g $v; set b $t}
				3 {set r $p; set g $q; set b $v}
				4 {set r $t; set g $p; set b $v}
				5 {set r $v; set g $p; set b $q}
			}
		}
		set rhex [dec_to_hex [expr $r*255]]
		set ghex [dec_to_hex [expr $g*255]]
		set bhex [dec_to_hex [expr $b*255]]
		return #$rhex$ghex$bhex
	}

	common dec_to_hex_map
	set dec_to_hex_map(0) 0
	set dec_to_hex_map(1) 1
	set dec_to_hex_map(2) 2
	set dec_to_hex_map(3) 3
	set dec_to_hex_map(4) 4
	set dec_to_hex_map(5) 5
	set dec_to_hex_map(6) 6
	set dec_to_hex_map(7) 7
	set dec_to_hex_map(8) 8
	set dec_to_hex_map(9) 9
	set dec_to_hex_map(10) a
	set dec_to_hex_map(11) b
	set dec_to_hex_map(12) c
	set dec_to_hex_map(13) d
	set dec_to_hex_map(14) e
	set dec_to_hex_map(15) f

	proc dec_to_hex {val} {
		set val [lindex [split $val .] 0]
		if {$val < 0} {set val 0}
		if {$val > 255} {set val 255}

		set dig1 [expr $val/16]
		set dig2 [expr $val-$dig1*16]
		return $dec_to_hex_map($dig1)$dec_to_hex_map($dig2)
	}

	#
	#  PUBLIC DATA
	#    -borderwidth ... width of border around entire widget
	#    -size .......... size of the HS editor window
	#
	public borderwidth 5 {
		if {$borderwidth < 0} {set borderwidth 0}
		if {[winfo exists $this]} {
			$this-win- config -borderwidth $borderwidth
		}
	}
	public size "2i" {
		if {[winfo exists $this]} {
			$this.s.r.valHS config -width $size -height $size
			_drawHS
		}
	}

	#
	#  PROTECTED DATA
	#    HSsize .......... radius for the HS circle
	#    valH ............ current value for color Hue
	#    valS ............ current value for color Saturation
	#    valV ............ current value for color Value
	#
	protected HSsize {}
	protected valH 0
	protected valS 0
	protected valV 0
}
