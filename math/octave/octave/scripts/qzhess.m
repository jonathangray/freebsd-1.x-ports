function [aa, bb, q, z] = qzhess (a, b)

# Usage: [aa, bb, q, z] = qzhess (a, b)
#
# Compute the qz decomposition of the matrix pencil (a - lambda b)
#
# result: (for Matlab compatibility):
#
#   aa = q*a*z and bb = q*b*z, with q, z orthogonal, and
#   v = matrix of generalized eigenvectors.
#
# This ought to be done in a compiled program
#
# Algorithm taken from Golub and Van Loan, Matrix Computations, 2nd ed.

# Written by A. S. Hodel (scotte@eng.auburn.edu) August 1993.

  if (nargin != 2)
    error ("usage: [aa, bb, q, z] = qzhess (a, b)");
  endif

  [na, ma] = size (a);
  [nb, mb] = size (b);
  if (na != ma || na != nb || nb != mb)
    error ("qzhess: incompatible dimensions");
  endif

# Reduce to hessenberg-triangular form.

  [q, bb] = qr (b);
  aa = q' * a;
  q = q';
  z = eye (na);
  for j = 1:(na-2)
    for i = na:-1:(j+2)

# disp (["zero out aa(", num2str(i), ",", num2str(j), ")"])

      rot = givens (aa (i-1, j), aa (i, j));
      aa ((i-1):i, :) = rot *aa ((i-1):i, :);
      bb ((i-1):i, :) = rot *bb ((i-1):i, :);
      q  ((i-1):i, :) = rot *q  ((i-1):i, :);

# disp (["now zero out bb(", num2str(i), ",", num2str(i-1), ")"])

      rot = givens (bb (i, i), bb (i, i-1))';
      bb (:, (i-1):i) = bb (:, (i-1):i) * rot';
      aa (:, (i-1):i) = aa (:, (i-1):i) * rot';
      z  (:, (i-1):i) = z  (:, (i-1):i) * rot';

    endfor
  endfor

  bb (2, 1) = 0.0;
  for i = 3:na
    bb (i, 1:(i-1)) = zeros (1, i-1);
    aa (i, 1:(i-2)) = zeros (1, i-2);
  endfor

endfunction
