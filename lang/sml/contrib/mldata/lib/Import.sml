




functor Import() : sig val Import: string->unit end  =
struct
   structure SysIO = System.Unsafe.SysIO;

   infix 9 endswith without;

   exception without_  ;

   fun s endswith t =
   size s >= size t andalso substring (s, size s - size t, size t) = t;

   fun s without t =
   if s endswith t then substring (s, 0, size s - size t) else raise without_;

   fun timeModified filename =
   let val System.Timer.TIME {sec, ...} = SysIO.mtime (SysIO.PATH filename)
   in
       sec
   end
   handle
      _ => 0;

   fun loadtarget root =
   let open System.Compile in loadTarget (root ^ ".bin"); openEnv () end;

   fun loadsource root =
   let open System.Compile in loadSource (root ^ ".bin"); openEnv () end;

   fun Rem a file b = app (outputc std_err) ["[Import ", a, " ", file, " ", b, "]\n"]

   fun Import objectname =
   let open System.Compile
       val  root = if objectname endswith ".sml" then objectname without ".sml" else objectname
       val  sml  = root ^ ".sml"
       val  bin  = root ^ ".bin"
       val  smltime = timeModified sml
       val  bintime = timeModified bin
   in
       if smltime = 0 then
	  Rem "cannot open" sml ""
       else
       if bintime < smltime orelse not(validTarget bin) then
	  (compile (sml, bin);
	   openEnv ()
	  )
       else
	  (loadTarget bin;
	   openEnv()
	  )

   end
end;
