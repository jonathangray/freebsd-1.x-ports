(*
% ForML Version 0.6 - 25 January 1993 - er@cs.cmu.edu
%************************************************************************
{\bf File {\tt load.sml} performs the straighforward
 loading and linking of the formatter.}
%************************************************************************
*)
use "formatter.sig";  (* defines signature FORMATTER *)
use "formatter.fun";  (* defines functor Formatter : FORMATTER *)

(*
If you want to use the formatter you might want to issue an
{\tt open Formatter} or {\tt local open Formatter in .. end} after
linking.
*)
structure Formatter : FORMATTER = Formatter();
