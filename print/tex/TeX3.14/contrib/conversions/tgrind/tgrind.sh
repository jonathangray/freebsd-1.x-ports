#! /bin/csh
# Script to grind nice program listings using TeX.
#
# written Feb, 1985 by Van Jacobson, Lawrence Berkeley Laboratory (adapted
# from the 4.2bsd "vgrind" script).
#
# Since TeX output handling is site dependent, you'll have to edit this
# file to get output to your local typesetting device(s).  Our site uses
# the flags "-v" (versatec output), "-q" (qms output) and "-k" (keep dvi file)
# to route output.  Put something appropriate to your site at the "PUT OUTPUT
# HANDLING..." comment at the end of this script.  If you've already dealt
# with this in your local tex command, just change the -v/q/k (or whatever)
# cases in the first "switch" to set variable "texoptions" appropriately.
#
set b=/usr/local/lib/tfontedpr
set tex=tex
set options=
set texoptions=
set files=
set head=""
set format=""
set output="ver"
top:
if ($#argv > 0) then
    switch ($1:q)

    case -d:
	if ($#argv < 2) then
	    echo "tgrind: $1:q option must have argument"
	    goto done
	else
	    set options = ($options $1:q $2)
	    shift
	    shift
	    goto top
	endif
			
    case -f:
	set format="Y"
	shift
	goto top

    case -h:
	if ($#argv < 2) then
	    echo "tgrind: $1:q option must have argument"
	    goto done
	else
	    set head="$2"
	    shift
	    shift
	    goto top
	endif
			
    case -v:
	set output="ver"
	shift
	goto top

    case -q:
	set output="qms"
	shift
	goto top

    case -k:
	set output=""
	shift
	goto top

    case -*:
	set options = "$options $1:q"
	shift
	goto top

    default:
	set files = "$files $1:q"
	shift
	goto top
    endsw
endif

if ("$format" == "Y") then
    if ("$head" != "") then
	$b $options -h "$head" $files
    else
	$b $options $files
    endif
    goto done
endif

if ("$head" != "") then
    $b $options -h "$head" $files >$files.tex
else
    $b $options $files >$files.tex
endif
$tex $texoptions $files.tex

# PUT OUTPUT HANDLING COMMANDS HERE.
done:
