(* Copyright 1989 by AT&T Bell Laboratories *)
signature REF = 
  sig
    val ! : 'a ref -> 'a
    val := : 'a ref * 'a -> unit
    val inc : int ref -> unit
    val dec : int ref -> unit
  end

signature LIST =
  sig
    datatype 'a list = :: of ('a * 'a list) | nil
    exception Hd
    exception Tl
    exception Nth
    exception NthTail
    val hd : 'a list -> 'a
    val tl : 'a list -> 'a list 
    val null : 'a list -> bool 
    val length : 'a list -> int 
    val @ : 'a list * 'a list -> 'a list
    val rev : 'a list -> 'a list 
    val map :  ('a -> 'b) -> 'a list -> 'b list
    val fold : (('a * 'b) -> 'b) -> 'a list -> 'b -> 'b
    val revfold : (('a * 'b) -> 'b) -> 'a list -> 'b -> 'b
    val app : ('a -> 'b) -> 'a list -> unit
    val revapp : ('a -> 'b) -> 'a list -> unit
    val nth : 'a list * int -> 'a
    val nthtail : 'a list * int -> 'a list
    val exists : ('a -> bool) -> 'a list -> bool
  end

signature VECTOR = 
  sig
      eqtype 'a vector
      exception Size
      exception Subscript
      val vector: 'a list -> 'a vector
      val tabulate: int * (int -> 'a) -> 'a vector
      val sub: 'a vector * int -> 'a 
      val length: 'a vector -> int
  end

signature ARRAY =
  sig
    type 'a array
    exception Size
    exception Subscript
    val array : int * '1a -> '1a array
    val arrayoflist : '1a list -> '1a array
    val tabulate: int * (int -> '1a) -> '1a array
    val sub : 'a array * int -> 'a
    val update : 'a array * int * 'a -> unit
    val length : 'a array -> int
  end

signature REAL_ARRAY = 
  sig
      eqtype realarray
      exception RealSubscript
      exception Size
      val length: realarray -> int
      val array: int * real -> realarray
      val sub: realarray * int -> real
      val update: realarray * int * real -> unit
  end

signature BYTEARRAY =
  sig
    eqtype bytearray
    exception Size
    exception Subscript
    exception Range
    val array : int * int -> bytearray
    val sub : bytearray * int -> int
    val update : bytearray * int * int -> unit
    val length : bytearray -> int
    val extract : bytearray * int * int -> string
    val fold : ((int * 'b) -> 'b) -> bytearray -> 'b -> 'b
    val revfold : ((int * 'b) -> 'b) -> bytearray -> 'b -> 'b
    val app : (int -> 'a) -> bytearray -> unit
    val revapp : (int -> 'b) -> bytearray -> unit
  end

signature IO =
  sig
    type instream 
    type outstream
    exception Io of string
    val std_in : instream
    val std_out : outstream
    val std_err : outstream
    val open_in : string -> instream
    val open_out : string -> outstream
    val open_append : string -> outstream
    val open_string : string -> instream
    val close_in : instream -> unit
    val close_out : outstream -> unit
    val output : outstream * string -> unit
    val outputc : outstream -> string -> unit
    val input : instream * int -> string
    val inputc : instream -> int -> string
    val input_line : instream -> string
    val lookahead : instream -> string
    val end_of_stream : instream -> bool
    val can_input : instream -> int
    val flush_out : outstream -> unit
    val is_term_in : instream -> bool
    val is_term_out : outstream -> bool
    val set_term_in : instream * bool -> unit
    val set_term_out : outstream * bool -> unit
    val execute : (string * string list) -> instream * outstream
    val execute_in_env : string * string list * string list
	                 -> instream * outstream
    val exportML : string -> bool
    val exportFn : string * (string list * string list -> unit) -> unit
  end

signature BOOL =
  sig
    datatype bool = true | false
    datatype 'a option = NONE | SOME of 'a  (* stuck in here for convenience *)
    val not : bool -> bool
    val print : bool -> unit
    val makestring : bool -> string
  end

signature STRING =
  sig
    type string
    exception Substring
    val length : string -> int
    val size : string -> int
    val substring : string * int * int -> string
    val explode : string -> string list
    val implode : string list -> string
    val <= : string * string -> bool
    val <  : string * string -> bool
    val >= : string * string -> bool
    val >  : string * string -> bool
    val ^  : string * string -> string
    exception Chr
    val chr : int -> string 
    exception Ord
    val ord : string -> int 
    val ordof : string * int -> int 
    val print : string -> unit
  end

signature INTEGER = 
  sig
    exception Sum and Diff and Prod and Neg and Quot and Abs
    exception Div and Mod
    exception Overflow
    type int
    val ~ : int -> int
    val * : int * int -> int
    val div : int * int -> int
    val mod : int * int -> int
    val quot : int * int -> int
    val rem : int * int -> int
    val + : int * int -> int
    val - : int * int -> int
    val >  : int * int -> bool
    val >= : int * int -> bool
    val <  : int * int -> bool
    val <= : int * int -> bool
    val min : int * int -> int
    val max : int * int -> int
    val abs : int -> int
    val print : int -> unit
    val makestring : int -> string
  end

signature BITS =
  sig
    type int
    val orb : int * int -> int
    val andb : int * int -> int
    val xorb : int * int -> int
    val lshift : int * int -> int
    val rshift : int * int -> int
    val notb :  int -> int
  end

signature REAL =
  sig
    type real
    exception Sum and Diff and Prod
    exception Floor and Sqrt and Exp and Ln
    exception Div
    exception Overflow
    val ~ : real -> real 
    val + : (real * real) -> real 
    val - : (real * real) -> real 
    val * : (real * real) -> real 
    val / : (real * real) -> real 
    val > : (real * real) -> bool
    val < : (real * real) -> bool
    val >= : (real * real) -> bool
    val <= : (real * real) -> bool
    val abs : real ->  real
    val real : int -> real
    val floor : real -> int
    val truncate : real -> int
    val ceiling : real -> int
    val sqrt : real -> real
    val sin : real -> real
    val cos : real -> real
    val arctan : real -> real
    val exp : real -> real
    val ln : real -> real
    val print : real -> unit
    val makestring : real -> string
  end

signature GENERAL =
  sig
    exception Bind
    exception Match
    exception Interrupt
      (* NOTE: Interrupt is never raised by the system, but is included to
       * provide some compatibility with the definition. *)
    exception Fail of string

    val use : string -> unit

    type 'a cont
    val callcc : ('1a cont -> '1a) -> '1a
    val throw : 'a cont -> 'a -> 'b

    val o : ('b -> 'c) * ('a -> 'b) -> ('a -> 'c)
    val before : ('a * 'b) -> 'a

    type exn
    type unit
    val = : ''a * ''a -> bool
    val <> : ''a * ''a -> bool

    datatype 'a frag = QUOTE of string | ANTIQUOTE of 'a 
  end
