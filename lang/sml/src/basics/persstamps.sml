
(* Copyright 1992 by AT&T Bell Laboratories *)
(* persstamps.sml *)

abstraction PersStamps : PERSSTAMPS =
struct

  (* Persistent stamp.
   * This is implemented as a pair. The second (string) component, shared from
   * the value set dynamically in xid, uniquely identifies a given execution of
   * the compiler. The first (integer) component is a simple counter, incremented
   * once for each stamp created during the current execution.
   *
   * The xid component is string concatenated from the string returned by 
   * gethostid, a two-byte string corresponding to the lower 16 bits of the 
   * current Unix process id, and a 4-byte string corresponding to the current
   * system time in seconds. Gethostid should be a tag uniquely identifying a
   * machine of a given manufacturer. (Commonly, it is set to the machine's
   * Internet address.) The second and third component should uniquely identify
   * a process running on that machine. Unix process ids range from 0 to 30000,
   * so we use two bytes for this. As process ids cycle, we use an additional
   * four bytes to capture the system time in seconds (elapsed time since 1/1/70).
   *
   * Assumptions and holes:
   *   - a machine won't cycle process ids in less than a second.
   *   - the system time won't be set incorrectly.
   *   - the compiler will only compare stamps generated from compilers running
   *     on the same machine type/OS/manufacturer, which can be distinguised using
   *     gethostid. In particular, cross-compilation could, in theory, be
   *     troublesome.
   *   - processes associated with the same gethostid value are distinguishable
   *     by (process id * time). In particular, problems could arise from a
   *     multiprocessor machine, with a single internet address used for gethostid,
   *     running separate copies of Unix on each processor.
   *)
  type persstamp = int * string

  val nextStamp = ref 1
  val xid : string ref = ref ""

  (* initXid:
   * Initialize xid uniquely for each execution of the compiler.
   *)
  fun initXid () = let
    fun intBytes2 i = 
      map chr [Bits.andb(Bits.rshift(i,8),0xFF), Bits.andb(i,0xFF)]
    fun intBytes i = (intBytes2 (Bits.rshift(i,16)))@(intBytes2 i)

    val hostid = System.Unsafe.CInterface.gethostid ()
    val pid = System.Unsafe.CInterface.getpid ()
    val System.Timer.TIME{sec,...} = System.Unsafe.CInterface.gettimeofday ()
  in
    xid := implode((intBytes2 pid)@(intBytes sec)@[hostid])
  end

  fun newStamp () = (
    case !xid of "" => initXid () | _ => ();
    (!nextStamp before inc nextStamp,!xid)
  )

  (* Total order on persistent stamps; we use a lexicographic ordering. *)
  fun less ((i,s):persstamp,(i',s')) = (i < i') orelse ((i = i') andalso (s < s'))
  fun greater ((i,s):persstamp,(i',s')) = (i > i') orelse ((i = i') andalso (s > s'))

  (* How should the xid component be used in the string representation? *)
  fun stampToString ((i,s):persstamp) = makestring i

  abstype 'a stampMap = STAMPMAP of 'a IntStrMap.intstrmap
  with
     fun newMap ex = STAMPMAP(IntStrMap.new (20, ex))

     fun updateMap (STAMPMAP map) ((i,s),v) = IntStrMap.add map (i, s, v)

     fun applyMap (STAMPMAP map, arg) = IntStrMap.map map arg
  end
    
end (* structure PersStamps *)
