(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Drew Dean <rdd@cs.cmu.edu>
   School of Computer Science, Carnegie-Mellon Univ., Pittsburgh, PA 15218 *)

structure GetEnv :GETENV = struct

exception GetEnv  (* raised by getenv if name is undefined in environment *)

fun getenv name =
  let val envVar = name ^ "="
      val envVar_size = size envVar
      val environment = System.Unsafe.CInterface.environ()
      fun getenv' str str_len [] = raise GetEnv
        | getenv' str str_len (ev::ev_list) =
          let val ev_len = size ev in
            if ev_len >= str_len andalso substring(ev, 0, str_len) = str 
              then substring(ev, str_len, ev_len-str_len)
              else getenv' str str_len ev_list
          end
  in
    getenv' envVar envVar_size environment
  end

fun printenv () =
  (map (fn s => output(std_out, (s ^ "\n")))
       (System.Unsafe.CInterface.environ());
   ())

end
