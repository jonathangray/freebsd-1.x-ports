(* background.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * Background widget.
 *)

signature BACKGROUND = 
  sig

    structure W : WIDGET

    type background

    val mkBackground : {
          color : W.EXB.color option,
          widget : W.widget
        } -> background

    val widgetOf : background -> W.widget

  end (* BACKGROUND *)

structure Background : BACKGROUND = 
  struct

    structure W = Widget

    type background = W.widget

    fun mkBackground {color, widget} = let
          open Geometry Interact
          val root = W.rootOf widget
          val color = 
            case color of 
              NONE => W.EXB.whiteOfScr (W.screenOf root)
            | SOME color => color
  
          fun realize {env, win, sz} = let
                val (my_inenv, my_outenv) = createWinEnv ()
                val (cinenv, coutenv) = createWinEnv ()
                val OutEnv{co=childco,...} = coutenv
                val InEnv{ci,co,...} = ignoreInput my_inenv
                val cwin = W.EXW.createSimpleSubwin win {
                      geom = WGEOM{pos=originPt, sz=sz, border=0},
                      backgrnd = SOME color,
                      border = NONE  (* not used *)
                    }

                fun handleCI (CI_Resize (RECT{x,y,wid,ht})) =
                      W.EXW.resizeWin cwin (SIZE{wid=wid,ht=ht})
                  | handleCI _ = ()
  
                fun handleCO CO_ResizeReq = CML.sync(co CO_ResizeReq)
                  | handleCO CO_KillReq = W.EXW.destroyWin cwin

                fun loop () =
                      CML.select [
                        CML.wrap (ci, loop o handleCI o msgBodyOf),
                        CML.wrap (childco, loop o handleCO)
                      ]
                in
                  Router.routePair (env, my_outenv, coutenv);
                  W.realizeFn widget {
                    env = cinenv, 
                    win = cwin,
                    sz = sz
                  };
                  W.EXW.mapWin cwin;
                  CML.spawn loop;
                  ()
                end
          in
            W.mkWidget{
                root=root, 
                boundsOf = W.boundsFn widget,
                realize=realize
              }
          end

    fun widgetOf w = w

  end (* Background *)

