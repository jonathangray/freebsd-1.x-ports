(* build.sml
 *
 * COPYRIGHT (c) 1990 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * Apply the CML functors.
 *)

abstraction CML : sig  (* hide the internals, but preserve type equality *)
    structure CML : sig
        include CONCUR_ML
        sharing type thread_id = CML.thread_id
	    and type chan = CML.chan
	    and type event = CML.event
	    and type time = CML.time = System.Timer.time
      end
    structure RunCML : RUN_CML
    structure CIO : CONCUR_IO
    structure TraceCML : TRACE_CML
    sharing CML = RunCML.CML = CIO.CML = TraceCML.CML
  end = struct
    structure CML = CML
    structure RunCML = RunCML(CML);
    structure CIO = ConcurIO(RunCML);
    structure TraceCML = TraceCML(
        structure CML = CML
              and RunCML = RunCML
              and CIO = CIO);
  end; (* CML *)

structure RunCML = CML.RunCML;
structure CIO = CML.CIO;
structure TraceCML = CML.TraceCML;
structure CML = CML.CML;

(* remove internal signatures and structures *)
signature CML_BASE = sig end;
signature INTERNAL_CML = sig end;
structure CMLVersion = struct end;
structure CMLBase = struct end;

