(* canvas.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * Simple canvas widget, serving as a template for
 * an application-dependent widget.
 *)

structure Canvas : CANVAS = 
  struct

    structure W = Widget
    structure D = Drawing

    local open CML W.G W.EXB W.EXW W.Interact Drawing Widget in

    datatype req_msg = 
      GetSize
    | DoRealize of {
        env : in_env,
        win : window,
        sz : size
      }

    datatype canvas = Canvas of {
      widget : widget,
      reqc : req_msg chan,
      repc : size chan,
      winvar : window cond_var
    }

    fun mkCanvas root bounds = let
          val reqChan = channel () and repChan = channel ()
          val winvar = condVar ()

          val (canvasInenv, OutEnv{m=om,k=ok,ci=oci,co=oco}) = createWinEnv ()
          val {x_dim,y_dim} = bounds
          val natx = natDim x_dim
          val naty = natDim y_dim
          val initSize = SIZE{wid=natx,ht=naty}

          fun realize {env=InEnv{m,k,ci,co}, win, sz=sz as SIZE{wid,ht}} = let
                val reqevt = receive reqChan

                fun handleReq (DoRealize _,sz) = sz
                  | handleReq (GetSize,sz) = (send (repChan, sz); sz)

                fun handleCI (msg,sz) =
                      case msgBodyOf msg of
                        CI_Resize (RECT{wid,ht,...}) => 
                          (sync(oci msg); SIZE{wid=wid,ht=ht})
                      | _ => (sync(oci msg);sz)

                fun loop sz = 
                      loop (select [
                        wrap (reqevt, (fn evt => handleReq(evt,sz))),
                        wrap (ci, (fn evt => handleCI(evt,sz)))
                      ])

                fun mkPipe (inc, outc) = let
                      fun loop () = loop(sync(outc(sync inc)))
                      in
                        spawn loop
                      end

              in
                mkPipe(m,om); mkPipe(k,ok); mkPipe(oco,co);
                writeVar (winvar, win);
                loop sz
              end

          val widget = mkWidget {
                root=root,
                boundsOf=fn () => bounds,
                realize = fn arg => send (reqChan, DoRealize arg)
              }

          fun canvasProc () = 
                case accept reqChan of
                  GetSize => (send (repChan, initSize); canvasProc ())
                | DoRealize arg => realize arg
      
          val canvas = 
                Canvas {
                  widget = widget,
                  reqc = reqChan,
                  repc = repChan,
                  winvar = winvar
                }
          in
            spawn canvasProc;
            (canvas, initSize, canvasInenv)
          end

    fun widgetOf (Canvas{widget,...}) = widget

    fun sizeOf (Canvas{reqc,repc,...}) = (send (reqc,GetSize); accept repc)

    fun drawableOfCanvas (Canvas{winvar,...}) = drawableOfWin (readVar winvar)

  end (* local *)
  end (* Canvas *)
