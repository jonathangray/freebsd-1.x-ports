(* This file compiles ML-Twig, translates the sample specification
   example.mtw to SML and loads it, and finally compiles a simple
   arithmetic expression to a sequence of instructions. *)

use "load.sml";

Main.main "example.mtw";

use "runtime.sml";
use "example.mtw.sml";

open TreeProcessor;
open TreeProcessor.User;

translate (Tree (Tree (Leaf 1,Mul,Leaf 2),Plus,Leaf 3));
