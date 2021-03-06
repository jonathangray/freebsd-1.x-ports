function zr = tzero (a, b, c, d, bal)

# Usage: zr = tzero (a, b, c, d, bal)
#
# Compute the transmission zeros of a, b, c, d.
#
# bal = balancing option (see balance); default is "B".
#
# Needs to incorporate mvzero algorithm to isolate finite zeros.

# Written by A. S. Hodel (scotte@eng.auburn.edu) August 1993.

  if (nargin == 4)
    bal = "B";
  elseif (nargin != 5)
    error ("tzero: illegal number of arguments");
  endif

  [n, m, p] = abcddim (a, b, c, d);

  if (n > 0 && m > 0 && p > 0)
    if (m != p)
      fprintf (stderr "tzero: number of inputs,outputs differ.  squaring up");
      if (p > m)
	fprintf (stderr, "       by padding b and d with zeros.");
	b = [b, zeros (n, p-m)];
	d = [d, zeros (p, p-m)];
	m = p;
      else
	fprintf (stderr, "       by padding c and d with zeros.");
	c = [c; zeros (m-p, n)];
	d = [d; zeros (m-p, m)];
	p = m;
      endif
      fprintf (stderr, "This is a kludge.  Try again with SISO system.");
    endif
    ab = [-a, -b; c, d];
    bb = [eye (n), zeros (n, m); zeros (p, n), zeros (p, m)];
    [ab,bb] = balance (ab, bb);
    zr = -qzval (ab, bb);
  else
    error ("tzero: a, b, c, d not compatible.  exiting");
  endif

endfunction
