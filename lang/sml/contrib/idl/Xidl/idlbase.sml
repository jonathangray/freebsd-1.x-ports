(*
	SML Base for IDL servers
	Bernard Sufrin,
	Oxford.
	@(#)idlbase.sml	2.1 93/03/07 00:58:11
*)

import "idlbase.sig";
functor idlbase() : idlbase =
struct
   datatype address = address of int * int ;
   type     short   = int ;
   type     byte    = int ;

   exception server_input_terminated;
   exception server_output_terminated;

   val (infromserver, outtoserver) = (ref std_in, ref std_out);
   val outwaiting = ref false;

   val running    = ref false;
   val servername = ref "<no server>";

   fun stopserver () =
   if !running then
   (close_in (! infromserver) handle _ => ();
    close_out (! outtoserver) handle _ => ();
    running := false)
   else
   ()

   fun flush () =
       (if !outwaiting then flush_out (! outtoserver) else ();
	outwaiting := false
       )
   handle Io s =>
   (   output(std_err, "[WARNING: Server "^ !servername ^" died]\n");
       stopserver()
   );

   fun bytein () = ord(input(!infromserver, 1))
		   handle Ord => raise server_input_terminated;

   fun write_int i =
   let val rs = Bits.rshift;
       val an = Bits.andb;
       val ref outtoserver = outtoserver
       fun f n = an (rs (i, n), 255)
   in
       output(outtoserver, chr (f 24));
       output(outtoserver, chr (f 16));
       output(outtoserver, chr (f 8));
       output(outtoserver, chr (an (i, 255)));
       outwaiting := true
   end
   handle Io _ => raise server_output_terminated;

   fun write_short i =
   let val rs = Bits.rshift;
       val an = Bits.andb;
       val ref outtoserver = outtoserver
       fun f n = an (rs (i, n), 255)
   in
       output(outtoserver, chr (f 8));
       output(outtoserver, chr (an (i, 255)));
       outwaiting := true
   end
   handle Io _ => raise server_output_terminated;

   fun write_address (address (hi, lo)) =
   let val rs = Bits.rshift;
       val an = Bits.andb;
       val ref outtoserver = outtoserver
       infix 1 an;
       infix 2 rs
   in
       output(outtoserver, chr (hi rs 8 an 255));
       output(outtoserver, chr (hi an 255));
       output(outtoserver, chr (lo rs 8 an 255));
       output(outtoserver, chr (lo an 255));
       outwaiting := true
   end
   handle Io _ => raise server_output_terminated;

   fun write_bool b =
       (output (! outtoserver, chr (if b then 1 else 0));
	outwaiting := true
       )
   handle Io _ => raise server_output_terminated;

   fun write_byte b =
       (output (! outtoserver, chr b);
	outwaiting := true
       )
   handle Io _ => raise server_output_terminated;

   fun write_string s =
   (write_int (size s);
    output (! outtoserver, s);
    outwaiting := true
   )

   fun write_real r = write_string (makestring (r : real));

   fun write_unit () =
       (output (! outtoserver, chr (0));
	outwaiting := true
       )
   handle Io _ => raise server_output_terminated;

   fun read_int () =
   let val _ = flush ();
       val b0 = bytein();
       val b1 = bytein();
       val b2 = bytein();
       val b3 = bytein();
       val ls = Bits.lshift;
       val orb= Bits.orb
       infix 9 ls
       infix 8 orb
   in
       b0 ls 24 orb b1 ls 16 orb b2 ls 8 orb b3
   end;

   fun read_short () =
   let val _ = flush ();
       val b2 = bytein();
       val b3 = bytein();
       val ls = Bits.lshift;
       val orb= Bits.orb
       infix 9 ls
       infix 8 orb
   in
       b2 ls 8 orb b3
   end;

   fun read_address () =
   let val _ = flush ();
       val b0 = bytein();
       val b1 = bytein();
       val b2 = bytein();
       val b3 = bytein();
       val ls = Bits.lshift;
       infix 9 ls
   in
       address (b0 ls 8 + b1, b2 ls 8 + b3)
   end;

   fun read_bool ()   = (flush (); bytein() <> 0);

   fun read_byte ()   = (flush (); bytein());

   fun read_string () = input (! infromserver, read_int ());

   fun read_unit ()   = (flush (); bytein(); ());

   fun startserver server args =
   let
       val (iii, ooo) = (stopserver(); IO.execute (server, args))
   in
       servername := server;
       infromserver := iii;
       outtoserver := ooo;
       running := true
   end;


end;

