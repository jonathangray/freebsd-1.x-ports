
functor streamio() : streamio =
struct
    fun  SwapStream(loc as (ref old)) new = (loc := new; old);
    val  instream       = ref std_in;
    val  outstream      = ref std_out;
    val  errstream      = ref std_err;
    val  writestring: string -> unit  = fn v   =>  outputc ( !outstream) v;
    val  write:      string -> unit   = fn v   =>  outputc ( !outstream) v;
    val  writeint:   int->unit        = fn v   =>  writestring (makestring v);
    val  writereal:  real->unit       = fn v   =>  writestring (makestring v);
    val  writebool:  bool->unit       = fn v   =>  writestring (makestring v);
    fun  writelist writeitem comma =
    let  fun  wl []       = ()
	 |    wl (x::xs)  =
	      (writeitem x; if null xs then () else (writestring comma; wl xs))
    in
	wl
    end;
    fun  writeoption witem  none = fn NONE => writestring none | SOME i =>  (witem i; ())
    fun  writef writer format items =
    let
	 fun  W 0 _  _ = ()
	 |    W n m items =
	      (case chr(ordof(format, m)) of
		   "%" => (writer (hd items); W (n-1) (m+1) (tl items))
	      |    "/" => (writestring(chr(ordof(format, m+1))); W(n-2)(m+2) items)
	      |    c   => (writestring c; W (n-1) (m+1) items)
	      )
    in
	 W (String.length format) 0 items
    end;
    fun readline() =   input_line ( !instream);
    fun readch()   =   inputc ( !instream) 1;
    fun eof()      =   end_of_stream ( !instream);
end;
