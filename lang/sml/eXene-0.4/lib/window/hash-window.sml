(* hash-window.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * A hash table package for hashing on windows.
 *)

structure HashWindow : sig

    type 'a window_map

    exception WindowNotFound

  (* create a new table *)
    val newMap : unit -> '1a window_map
  (* insert an item *)
    val insert : '2a window_map -> (DrawTypes.window * '2a) -> unit
  (* find an item, the exception WindowNotFound is raised if the
   * item doesn't exist.
   *)
    val find : 'a window_map -> DrawTypes.window -> 'a
  (* find an item (using an id as the key), the exception WindowNotFound
   *is raised if the item doesn't exist *)
    val findWinId : 'a window_map -> XProtTypes.win_id -> 'a
  (* remove an item *)
    val remove : 'a window_map -> DrawTypes.window -> 'a
  (* return a list of the items in the table *)
    val list : 'a window_map -> 'a list

  end = struct
    local
      open DrawTypes
    in
    type 'a window_map = 'a HashXId.xid_map
    exception WindowNotFound = HashXId.XIdNotFound
    val newMap = HashXId.newMap
    fun find m (WIN{id, ...}) = HashXId.find m id
    val findWinId = HashXId.find
    fun insert m (WIN{id, ...}, a) = HashXId.insert m (id, a)
    fun remove m (WIN{id, ...}) = HashXId.remove m id
    val list = (map #2) o HashXId.list
    end
  end (* HashXId *)
