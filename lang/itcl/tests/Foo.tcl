#
# Test class for [incr Tcl] test suite
# ----------------------------------------------------------------------
#   AUTHOR:  Michael J. McLennan       Phone: (610)712-2842
#            AT&T Bell Laboratories   E-mail: michael.mclennan@att.com
#
#      RCS:  Foo.tcl,v 1.2 1994/04/25 19:15:06 mmc Exp
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

itcl_class Foo {
	#
	#  Constructor/destructor add their name to a global var for
	#  tracking implicit constructors/destructors
	#
	constructor {config} {
		global WATCH
		lappend WATCH [info class]
		set foos($this) $this
		incr nfoo
	}
	destructor {
		global WATCH
		lappend WATCH [info class]
		unset foos($this)
	}

	method nothing {} {}

	method do {cmds} {
		return "Foo says '[eval $cmds]'"
	}

	#
	#  Test formal arguments for methods/procs
	#  (formal args should not clobber data members)
	#
	method testMethodArgs {blit _blit args} {
		return "$blit, $_blit, and [llength $args] other args"
	}
	proc testProcArgs {nfoo args} {
		return "$nfoo, and [llength $args] other args"
	}

	#
	#  Test methods using the "config" argument
	#
	method config {{config -blit auto -blat matic}} {
		return $config
	}
	method xconfig {x config} {
		return "$x|$config"
	}
	method configx {config x} {
		return "$config|$x"
	}
	method xecho {x args} {
		return "$x | [llength $args]: $args"
	}

	#
	#  Test procs and access to common vars
	#
	proc echo {x args} {
		return "$x | [llength $args]: $args"
	}
	proc foos {{pattern *}} {
		set retn {}
		foreach i [array names foos] {
			if {$i != "_ignore_" && [string match $pattern $foos($i)]} {
				lappend retn $foos($i)
			}
		}
		return $retn
	}
	proc nfoos {} {
		return $nfoo
	}

	#
	#  Test public/protected/common variable definitions
	#
	public blit
	public blat 0
	public blot 1 {global WATCH; set WATCH "blot=$blot"}

	protected _blit
	protected _blat 0

	common foos
	set foos(_ignore_) "foos-is-now-an-array"

	common nfoo 0
}
