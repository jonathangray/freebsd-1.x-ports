(* shell.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * Shell widget to provide interface between X library/window manager
 * and widgets.
 *)

signature SHELL = 
  sig

    structure W : WIDGET

    type shell

    (* type wm_args = { 
     *   win_name : string option, 
     *   icon_name : string option
     * }
     *)
    type wm_args
    val mkWMArgs : {win_name : string option, icon_name : string option} -> wm_args

    (* type hints = {
     *   size_hints : size_hints list,
     *   wm_hints : wm_hints list
     * }
     *)
    type hints
    val mkHints : {size_hints : W.EXW.ICCC.size_hints list, wm_hints : W.EXW.ICCC.wm_hints list } 
                    -> hints

    val mkShell : W.widget * W.EXB.color option * wm_args -> shell
    val mkTransientShell : W.EXB.window -> 
          W.widget * W.EXB.color option * wm_args -> shell

    val setWMHints : shell -> hints -> unit
    val init : shell -> unit
    val destroy : shell -> unit

  end (* SHELL *)

structure Shell : SHELL = 
  struct

    structure W = Widget
    structure ICCC = ICCC

    local open CML Geometry EXeneBase Interact EXeneWin Widget ICCC in

    type hints = {
      size_hints : size_hints list,
      wm_hints : wm_hints list
      (* class_hints : {res_class : string, res_name : string} option *)
    }
    fun mkHints a = a
    datatype shell_msg = Init | Destroy | Map of bool | Hints of hints

    datatype shell = Shell of (shell_msg chan)
    
    fun setSizeHints {x_dim=x_dim as DIM xdim,y_dim=y_dim as DIM ydim} = let
          fun minSz () = let
                val minx = minDim x_dim
                val miny = minDim y_dim
                in
                  SIZE{wid=max(1,minx),ht=max(1,miny)}
                end
          fun maxSz () = (maxDim x_dim, maxDim y_dim)
          fun incSz () = (#incr xdim, #incr ydim)

          val MAX = 65535

          fun doInc () =
                case incSz () of
                  (1,1) => []
                | (x, 1) => [HINT_PResizeInc (SIZE{wid=x,ht=1})]
                | (1, y) => [HINT_PResizeInc (SIZE{wid=1,ht=y})]
                | (x, y) => [HINT_PResizeInc (SIZE{wid=x,ht=y})]

          fun doMin () = let
                val minsz = minSz ()
                in
                  [HINT_PMinSize minsz,HINT_PBaseSize minsz]
                end

          fun doMax () =
                case maxSz () of
                  (NONE,NONE) => []
                | (SOME x, NONE) => [HINT_PMaxSize (SIZE{wid=x,ht=MAX})]
                | (NONE, SOME y) => [HINT_PMaxSize (SIZE{wid=MAX,ht=y})]
                | (SOME x, SOME y) => [HINT_PMaxSize (SIZE{wid=x,ht=y})]
          in
            (doInc())@(doMax())@(doMin())
          end

(* DEBUG
    val setSizeHints = fn arg => let
          val pr = XDebug.pr1
          val arglist = setSizeHints arg
          fun pritem (HINT_PResizeInc sz) = pr("inc = "^(Db.sztos sz)^"\n")
            | pritem (HINT_PMaxSize sz) = pr("max = "^(Db.sztos sz)^"\n")
            | pritem (HINT_PMinSize sz) = pr("min = "^(Db.sztos sz)^"\n")
            | pritem _ = ()
          in
            app pritem arglist;
            arglist
          end
*)

    type wm_args = { win_name : string option, icon_name : string option }
    fun mkWMArgs a = a

    fun mk_shell crwin (widget, colorOpt, wm_args : wm_args) = let
          val root = rootOf widget
          val reqChan = channel ()
          val scr = screenOf root
          val color = 
            case colorOpt of 
              NONE => whiteOfScr scr
            | SOME color => color
  
          fun setProtocols win = 
                setWMProtocols win [ICCC.internAtom (displayOf root) "WM_DELETE_WINDOW"]
               
          fun init (hintlist,mapped) = let
                val bnds as {x_dim,y_dim} = boundsOf widget
                val size = SIZE{wid=natDim x_dim,ht=natDim y_dim}
                val (twin, inEnv) = crwin widget {
                  geom=WGEOM{pos=PT{x=0,y=0}, sz=size, border=0},
                  backgrnd = color,
                  border = color   (* not used *)
                }

                fun sendHint {size_hints, wm_hints} =
                      setWMProperties twin {
                        argv = [],
                        win_name = NONE,
                        icon_name = NONE,
                        size_hints = size_hints,
                        wm_hints = wm_hints,
                        class_hints = NONE
                      }

                val _ = setWMProperties twin {
                          argv = System.argv(),
                          win_name = #win_name wm_args,
                          icon_name = #icon_name wm_args,
                          size_hints = setSizeHints bnds,
                          wm_hints = [],
                          class_hints = NONE
                        }
                val _ = app sendHint (rev hintlist)
                val _ = setProtocols twin

                val (my_inenv, my_outenv) = createWinEnv ()
    
                val cwin = wrapCreate(twin, mkRect(originPt,size)) 
                val (cinenv, coutenv as OutEnv{co,...}) = createWinEnv ()
                val childco = wrapQueue co
                val InEnv{ci=myci,...} = ignoreInput my_inenv
    
                fun zombie () =
                      zombie (select [
                        wrap (myci, fn _ => ()),
                        wrap (receive reqChan, fn _ => ()),
                        wrap (childco, fn _ => ())
                      ])
          
                fun handleCO CO_ResizeReq = let
                      val {x_dim,y_dim} = boundsOf widget
                      in
                        resizeWin twin (SIZE{wid=natDim x_dim,ht=natDim y_dim})
                      end
                  | handleCO CO_KillReq = (destroyWin twin; zombie())
          
                fun handleCI (CI_Resize (RECT{wid,ht,...})) = 
                      resizeWin cwin (SIZE{wid=wid,ht=ht})
                  | handleCI CI_OwnDeath = zombie ()
                  | handleCI (CI_ChildDeath _) = zombie ()
                  | handleCI (CI_Redraw _) = ()
                  | handleCI _ = ()
          
                  (* send UnmapNotify event to Window mgr; see ICCCM *) 
                fun sendUnmapEvt () = ( (* unimplemented *) )

                fun mapTopWin (false,true) = (mapWin twin; true)
                  | mapTopWin (true, false) = 
                      (unmapWin twin; sendUnmapEvt(); false)
                  | mapTopWin (_,b) = b
                      
                fun handleReq mapped =
                      fn Init => mapped
                       | Destroy => (destroyWin twin; zombie ())
                       | Hints hint => (sendHint hint; mapped)
                       | Map arg => mapTopWin (mapped,arg)
        
                fun loop mapped =
                       (select [
                           wrap (myci, handleCI o msgBodyOf),
                           wrap (receive reqChan, loop o (handleReq mapped)),
                           wrap (childco, handleCO)
                         ]; 
                       loop mapped)
                in
                  Router.routePair (inEnv, my_outenv, coutenv);
                  realizeFn widget {
                    env = cinenv, 
                    win = cwin,
                    sz = size
                  };
                  mapWin cwin;
                  loop (mapTopWin(false, mapped))
                end
  
          fun initLoop (arg as (hintlist,mapped)) =
            case accept reqChan of
              Init => init arg
            | Destroy => initLoop arg
            | Hints hint => initLoop (hint::hintlist,mapped)
            | Map mapped' => initLoop (hintlist,mapped')
  
          in
            XDebug.xspawn ("shell", fn () => initLoop ([],true));
            Shell reqChan
          end
  
    val mkShell = 
          mk_shell (fn wdgt => createSimpleTopWin (screenOf(rootOf wdgt)))
    fun mkTransientShell w = mk_shell (fn _ => createTransientWin w)
    fun init (Shell ch) = send (ch, Init)
    fun destroy (Shell ch) = send (ch, Destroy)
    fun unmap (Shell ch) = send(ch, Map false)
    fun map (Shell ch) = send(ch, Map true)
    fun setWMHints (Shell ch) arg = send (ch, Hints arg)

    end (* local *)
  end (* Shell *)
