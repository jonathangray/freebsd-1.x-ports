(* winreg.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * TODO
 *   - refresh the keymap on ModifierMappingNotifyXEvt and KeyboardMappingNotifyXEvt
 *     events.
 *   - think about the relation of locks and changes in the tree structure; also
 *     locking already locked windows.
 *)

signature WIN_REGISTRY =
  sig

    type registry

    datatype path
      = PathDst of XProtTypes.win_id
      | Path of (XProtTypes.win_id * path)

    val createRegistry : (XDisplay.xdisplay * Keymap.keymap) -> registry

    val logTopWin : (registry * XProtTypes.win_id)
	  -> (path * XEventTypes.xevent) CML.event

  (* lock a window and all of its descendants; return the unlocking function *)
    val lockWinTree : (registry * XProtTypes.win_id) -> (unit -> unit)

  (* test to see if a window is locked *)
    val isLocked : (registry * XProtTypes.win_id) -> bool

  end (* WIN_REGISTRY *)

structure WinRegistry : WIN_REGISTRY =
  struct

    datatype path
      = PathDst of XProtTypes.win_id
      | Path of (XProtTypes.win_id * path)

    local
      open CML XEventTypes XDisplay

      datatype req_msg
	= REQ_new of XProtTypes.win_id
	| REQ_lock of XProtTypes.win_id
	| REQ_unlock of XProtTypes.win_id
	| REQ_islocked of XProtTypes.win_id

(** NOTE: could bundle this all into a couple of functions **)
      datatype win_desc = WD of {
	  id : XProtTypes.win_id,
	  path : path,
	  parent : win_desc option,
	  children : win_desc list ref,
	  lock : bool ref,
	  evt_strm : (path * xevent) chan
	}

      datatype xevt_dst
	= ToWindow of XProtTypes.win_id
	| CreateWin of {parent : XProtTypes.win_id, new_win : XProtTypes.win_id}
	| DestroyWin of XProtTypes.win_id
	| ToAll
	| ToTrash

    (* discard instances of an X-event that are the product of SubstructureNotify,
     * instead of StructureNotify. *)
      fun filterSubstructXEvt (w1, w2) = if (w1 = w2) then (ToWindow w1) else ToTrash

    (* extract the destination xid of an X-event *)
      fun extractDst (KeyPressXEvt{event, ...}) = ToWindow event
	| extractDst (KeyReleaseXEvt{event, ...}) = ToWindow event
	| extractDst (ButtonPressXEvt{event, ...}) = ToWindow event
	| extractDst (ButtonReleaseXEvt{event, ...}) = ToWindow event
	| extractDst (MotionNotifyXEvt{event, ...}) = ToWindow event
	| extractDst (EnterNotifyXEvt{event, ...}) = ToWindow event
	| extractDst (LeaveNotifyXEvt{event, ...}) = ToWindow event
	| extractDst (FocusInXEvt{event, ...}) = ToWindow event
	| extractDst (FocusOutXEvt{event, ...}) = ToWindow event
	(*| extractDst (KeymapNotifyXEvt{, ...}) = *)
	| extractDst (ExposeXEvt{window, ...}) = ToWindow window
	(*| extractDst (GraphicsExposeXEvt ?? *)
	(*| extractDst (NoExposeXEvt{, ...}) =*)
	| extractDst (VisibilityNotifyXEvt{window, ...}) = ToWindow window
	| extractDst (CreateNotifyXEvt{parent, window, ...}) =
	    CreateWin{parent = parent, new_win = window}
	| extractDst (DestroyNotifyXEvt{event, window, ...}) =
	    if (event = window)
	      then (DestroyWin event)	(* remove window from registry *)
	      else (ToWindow event)	(* report to parent that child is dead *)
	| extractDst (UnmapNotifyXEvt{event, window, ...}) =
	    filterSubstructXEvt(event, window)
	| extractDst (MapNotifyXEvt{event, window, ...}) =
	    filterSubstructXEvt(event, window)
	(*| extractDst (MapRequestXEvt{, ...}) =*)
	| extractDst (ReparentNotifyXEvt _) = ToTrash
	| extractDst (ConfigureNotifyXEvt{event, window, ...}) =
	    filterSubstructXEvt(event, window)
	(*| extractDst (ConfigureRequestXEvt{, ...}) =*)
	| extractDst (GravityNotifyXEvt{event, window, ...}) =
	    filterSubstructXEvt(event, window)
	(*| extractDst (ResizeRequestXEvt{, ...}) =*)
	| extractDst (CirculateNotifyXEvt{event, window, ...}) =
	    filterSubstructXEvt(event, window)
	(*| extractDst (CirculateRequestXEvt{, ...}) =*)
	| extractDst (PropertyNotifyXEvt{window, ...}) = ToWindow window
	| extractDst (SelectionClearXEvt{owner, ...}) = ToWindow owner
	| extractDst (SelectionRequestXEvt{owner, ...}) = ToWindow owner
	| extractDst (SelectionNotifyXEvt{requestor, ...}) = ToWindow requestor
	| extractDst (ColormapNotifyXEvt{window, ...}) = ToWindow window
	| extractDst (ClientMessageXEvt{window, ...}) = ToWindow window
	| extractDst ModifierMappingNotifyXEvt = ToAll
	| extractDst (KeyboardMappingNotifyXEvt _)  = ToAll
	| extractDst PointerMappingNotifyXEvt = ToAll
	| extractDst e = (
	    MLXError.warning(implode[
	      "[WinReg: unexpected ", XPrint.xevtName e, " event]\n"]);
	    ToTrash)
(* +DEBUG *)
local
  open XPrint
  fun dst2s ToTrash = "ToTrash"
    | dst2s ToAll = "ToAll"
    | dst2s (ToWindow w) = ("ToWindow(" ^ xidToString w ^ ")")
    | dst2s (CreateWin{parent, new_win}) = implode[
	  "CreateWin{parent=", xidToString parent, ", new_win=",
	  xidToString new_win, "}"
	]
    | dst2s (DestroyWin w) = ("DestroyWin(" ^ xidToString w ^ ")")
in
val extractDst = fn evt => let val dst = extractDst evt
      in
        XDebug.trace(XDebug.winregTM, fn () => ["WinReg: ", xevtName evt, " => ", dst2s dst, "\n"]);
        dst
      end
end
(* -DEBUG *)

    in

    datatype registry = WinReg of {
	reqch : req_msg chan,
	replych : (path * XEventTypes.xevent) event chan,
	lockch : bool chan
      }

    fun setLock v = let
	  fun set (WD{lock, children, ...}) = (lock := v; setList (!children))
	  and setList [] = ()
	    | setList (wd::r) = (set wd; setList r)
	  in
	    set
	  end
    val lockTree = setLock true
    val unlockTree = setLock false

    fun createRegistry (XDPY{conn, ...}, keymap) = let
	  val xevtIn = XIo.waitForXEvent conn
	  val registerReqCh = channel() and registerReplyCh = channel()
	  val lockReplyCh = channel()
	  val idMap = HashXId.newMap()
	  val find = HashXId.find idMap
	  val insert = HashXId.insert idMap
	  val remove = HashXId.remove idMap
	  fun handleReq (REQ_new win) = let (* log a new top-level window *)
		val evtCh = channel()
		in
		  insert (win, WD{
		      id = win, path = PathDst win, parent = NONE,
		      children = ref[], lock = ref false, evt_strm = evtCh
		    });
		  send(registerReplyCh, receive evtCh)
		end
	    | handleReq (REQ_lock win) = (lockTree (find win))
	    | handleReq (REQ_unlock win) = (unlockTree (find win))
	    | handleReq (REQ_islocked win) = let
		val WD{lock, ...} = find win
		in
		  send (lockReplyCh, !lock)
		end
	  fun newSubwin (parent, childId) = let
		val parentDesc as WD{path, evt_strm, children, lock, ...} = find parent
		fun extendPath (PathDst w) = Path(w, PathDst childId)
		  | extendPath (Path(w, path)) = Path(w, extendPath path)
		val childPath = extendPath path
		val child = WD{
			id = childId, path = childPath, parent = SOME parentDesc,
			children = ref [], lock = ref(! lock), evt_strm = evt_strm
		      }
		in
		  children := child :: (! children);
		  insert (childId, child)
		end
	  fun sendEvt (e, WD{path, evt_strm, ...}) = send (evt_strm, (path, e))
	  fun sendEvtToWin (e, winId) = (sendEvt (e, find winId))
		handle HashXId.XIdNotFound => ()
	  fun handleEvt e = (case (extractDst e)
		 of ToTrash => ()
		  | ToAll => app (fn (_, x) => sendEvt(e, x)) (HashXId.list idMap)
		  | (ToWindow winId) => sendEvtToWin(e, winId)
		  | (CreateWin{parent, new_win}) => (
		      newSubwin(parent, new_win);
		      sendEvtToWin(e, parent))
		  | (DestroyWin winId) => (case (remove winId)
		       of (win as WD{parent=SOME(WD{children,...}), ...}) => let
			    fun removeChild [] = (
				  MLXError.warning "[WinReg: missing child]"; [])
			      | removeChild ((w as WD{id, ...}) :: r) =
				  if (id = winId) then r else (w :: (removeChild r))
			    in
			      children := removeChild (! children);
			      sendEvt (e, win)
			    end
			| win => sendEvt (e, win)
		      (* end case *))
		(* end case *))
	  val evt = choose [
		  wrap (receive registerReqCh, handleReq),
		  wrap (xevtIn, handleEvt)
		]
	  (*fun loop () = (sync evt; loop())*)
(* DEBUG *) fun loop () = (XDebug.trace(XDebug.winregTM, fn () => ["Winreg.loop: waiting\n"]); sync evt; loop())
	  in
	    XDebug.xspawn ("WinReg", loop);
	    WinReg{
		reqch = registerReqCh,
		replych = registerReplyCh,
		lockch = lockReplyCh
	      }
	  end (* createRegistry *)

      fun logTopWin (WinReg{reqch, replych, ...}, win) = (
	    send(reqch, REQ_new win); accept replych)

      fun lockWinTree (WinReg{reqch, replych, ...}, win) = (
	    send (reqch, REQ_lock win);
	    fn () => send (reqch, REQ_unlock win))

      fun isLocked (WinReg{reqch, lockch, ...}, win) = (
	    send (reqch, REQ_islocked win);
	    accept lockch)

    end (* local *)
  end (* WinRegistry *)
