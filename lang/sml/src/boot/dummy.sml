(* Copyright 1989 by AT&T Bell Laboratories *)

abstraction Dummy : ASSEMBLY =
  struct
    datatype datalist = DATANIL | DATACONS of (string * (unit -> unit) * datalist)
    type func = unit
    datatype funclist = FUNCNIL | FUNC of (func * string * funclist)
    type object = unit
    structure AA = struct
      val array = InLine.cast()
      val callc = InLine.cast()
      type bytearray = string
      type realarray = string
      val create_b = InLine.cast()
      val create_r = InLine.cast()
      val create_s = InLine.cast()
(*      type 'a vector = 'a array*)
      val create_v = InLine.cast()
      val floor = InLine.cast()
      val logb = InLine.cast()
      val scalb =  InLine.cast()
      type spin_lock = unit
      val try_lock = InLine.cast()
      val unlock = InLine.cast()
    end
    structure A : RUNVEC = AA
    exception Div
    exception Overflow
    exception SysError of (int * string)
    exception UnboundTable
    val active_procs = InLine.cast()
    val array0 = InLine.cast()
    val bytearray0 = InLine.cast()
    val calleesaves = InLine.cast()
    val collected = InLine.cast()
    val collectedfrom = InLine.cast()
    val current = InLine.cast()
    val datalist = InLine.cast()
    val dtablesize = InLine.cast()
    val external = InLine.cast()
    val gcmessages = InLine.cast()
    val times = InLine.cast()
    val lastratio = InLine.cast()
    val machine = InLine.cast()
    val majorcollections = InLine.cast()
    val minorcollections = InLine.cast()
    val opsys = InLine.cast()
    val pstruct = InLine.cast()
    val ratio = InLine.cast()
    val realarray0 = InLine.cast()
    val sighandler = InLine.cast()
    val softmax = InLine.cast()
    val vector0 = InLine.cast()
end

structure Core = CoreFunc(Dummy)

