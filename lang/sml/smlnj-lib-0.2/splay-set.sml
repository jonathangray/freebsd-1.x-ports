(* splay-set.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Functor implementing ordered sets using splay trees.
 *
 *)

functor SplaySet (K : ORD_KEY) : ORD_SET =
  struct
    structure Key = K
    open LibBase K SplayTree

    type item = ord_key
  
    datatype set = 
      OS of {
        root : item splay ref,
        nobj : int
      }

    exception NotFound
    fun cmpf k = fn k' => cmpKey(k',k)

    val empty = OS{root = ref SplayNil, nobj = 0}
    fun singleton v = OS{root = ref(SplayObj{value=v,left=SplayNil,right=SplayNil}),nobj=1}
    
	(* Primitive insertion.
	 *)
    fun insert (v,(nobj,root)) =
          case splay (cmpf v, root) of
            (_,SplayNil) => 
              (1,SplayObj{value=v,left=SplayNil,right=SplayNil})
          | (Equal,SplayObj{value,left,right}) => 
              (nobj,SplayObj{value=v,left=left,right=right})
          | (Less,SplayObj{value,left,right}) => 
              (nobj+1,
               SplayObj{
                 value=v,
                 left=SplayObj{value=value,left=left,right=SplayNil},
                 right=right})
          | (Greater,SplayObj{value,left,right}) => 
              (nobj+1,
               SplayObj{
                  value=v,
                  left=left,
                  right=SplayObj{value=value,left=SplayNil,right=right}})

	(* Add an item.  
	 *)
    fun add (OS{root,nobj},v) = let
          val (cnt,t) = insert(v,(nobj,!root))
          in
            OS{nobj=cnt,root=ref t}
          end

	(* Insert a list of items.
	 *)
    fun addList (OS{root,nobj},l) = let
          val (cnt,t) = revfold insert l (nobj,!root)
          in
            OS{nobj=cnt,root=ref t}
          end

	(* Look for an item, return NONE if the item doesn't exist *)
    fun peek (d as OS{root,nobj},key) =
          case splay (cmpf key, !root) of
            (_,SplayNil) => NONE
          | (Equal,r as SplayObj{value,...}) => (root := r; SOME value)
          | (_,r) => (root := r; NONE)

	(* Find an item *)
    fun member arg = case peek arg of NONE => false | SOME _ => true

	(* Find an item, raising NotFound if not found *)
    fun find arg = case peek arg of NONE => raise NotFound | SOME v => v

	(* Remove an item.
         * Raise NotFound if not found
	 *)
    fun delete (OS{root,nobj},key) =
      case splay (cmpf key, !root) of
        (_,SplayNil) => raise NotFound
      | (Equal,SplayObj{value,left,right}) => 
          OS{root=ref(join(left,right)),nobj=nobj-1}
      | (_,r) => (root := r; raise NotFound)

	(* Return the number of items in the table *)
    fun numItems (OS{nobj,...}) = nobj

    fun isEmpty (OS{nobj=0,...}) = true
      | isEmpty _ = false

    local
      fun member (x,tree) = let
            fun mbr SplayNil = false
              | mbr (SplayObj{value,left,right}) =
                  case cmpKey(x,value) of
                    Less => mbr left
                  | Greater => mbr right
                  | _ => true
          in mbr tree end

        (* true if every item in t is in t' *)
      fun treeIn (t,t') = let
            fun isIn SplayNil = true
              | isIn (SplayObj{value,left=SplayNil,right=SplayNil}) =
                  member(value, t')
              | isIn (SplayObj{value,left,right=SplayNil}) =
                  member(value, t') andalso isIn left
              | isIn (SplayObj{value,left=SplayNil,right}) =
                  member(value, t') andalso isIn right
              | isIn (SplayObj{value,left,right}) =
                  member(value, t') andalso isIn left andalso isIn right
            in
              isIn t
            end
    in
    fun isSubset (OS{root=rt,nobj=n},OS{root=rt',nobj=n'}) =
          (n<=n') andalso treeIn (!rt,!rt')

    fun equal (OS{root=rt,nobj=n},OS{root=rt',nobj=n'}) =
          (n=n') andalso treeIn (!rt,!rt')
    end

    fun split (value,s) =
          case splay(cmpf value, s) of
            (Equal,SplayObj{value,left,right}) => (SOME value, left, right)
          | (Less,SplayObj{value,left,right}) => (NONE, SplayObj{value=value,left=left,right=SplayNil},right)
          | (Greater,SplayObj{value,left,right}) => (NONE, left, SplayObj{value=value,right=right,left=SplayNil})
          | (_,SplayNil) => (NONE, SplayNil, SplayNil)

    fun intersection (s as OS{nobj=0,...},_) = s
      | intersection (_,s as OS{nobj=0,...}) = s
      | intersection (OS{root,...},OS{root=root',...}) =
          let fun inter(SplayNil,_) = (SplayNil,0)
                | inter(_,SplayNil) = (SplayNil,0)
                | inter(s, SplayObj{value,left,right}) =
                    case split(value,s) of
                      (SOME v, l, r) =>
                        let val (l',lcnt) = inter(l,left)
                            val (r',rcnt) = inter(r,right)
                        in
                          (SplayObj{value=v,left=l',right=r'},lcnt+rcnt+1)
                        end
                    | (_,l,r) =>
                        let val (l',lcnt) = inter(l,left)
                            val (r',rcnt) = inter(r,right)
                        in
                          (join(l',r'),lcnt+rcnt)
                        end
              val (root,cnt) = inter(!root,!root')
          in
            OS{root = ref root, nobj = cnt}
          end

    fun count st =
         let fun cnt(SplayNil,n) = n
               | cnt(SplayObj{left,right,...},n) = cnt(left,cnt(right,n+1))
         in
           cnt(st,0)
         end

    fun difference (s as OS{nobj=0,...},_) = s
      | difference (s,OS{nobj=0,...}) = s
      | difference (OS{root,...}, OS{root=root',...}) =
          let fun diff(SplayNil,_) = (SplayNil,0)
                | diff(s,SplayNil) = (s, count s)
                | diff(s,SplayObj{value,right,left}) =
                    let val (_,l,r) = split(value,s)
                        val (l',lcnt) = diff(l,left)
                        val (r',rcnt) = diff(r,right)
                    in
                      (join(l',r'),lcnt+rcnt)
                    end
              val (root,cnt) = diff(!root,!root')
          in
            OS{root = ref root, nobj = cnt}
          end

    fun union (OS{nobj=0,...},s) = s
      | union (s,OS{nobj=0,...}) = s
      | union (OS{root,...}, OS{root=root',...}) =
          let fun uni(SplayNil,s) = (s,count s)
                | uni(s,SplayNil) = (s, count s)
                | uni(s,SplayObj{value,right,left}) =
                    let val (_,l,r) = split(value,s)
                        val (l',lcnt) = uni(l,left)
                        val (r',rcnt) = uni(r,right)
                    in
                      (SplayObj{value=value,right=r',left=l'},lcnt+rcnt+1)
                    end
              val (root,cnt) = uni(!root,!root')
          in
            OS{root = ref root, nobj = cnt}
          end

	(* Return a list of the items (and their keys) in the dictionary *)
    fun listItems (OS{root,...}) =
        let fun apply (SplayNil,l) = l
              | apply (SplayObj{value,left,right},l) =
                  apply(left, value::(apply (right,l)))
        in
          apply (!root,[])
        end

	(* Apply a function to the entries of the dictionary *)
    fun app af (OS{root,...}) =
          let fun apply SplayNil = ()
                | apply (SplayObj{value,left,right}) = 
                    (apply left; af value; apply right)
        in
          apply (!root)
        end

    fun revapp af (OS{root,...}) =
          let fun apply SplayNil = ()
                | apply (SplayObj{value,left,right}) = 
                    (apply right; af value; apply left)
        in
          apply (!root)
        end

	(* Fold function *)
    fun fold abf (OS{root,...}) b =
          let fun apply (SplayNil, b) = b
                | apply (SplayObj{value,left,right},b) =
                    apply(left,abf(value,apply(right,b)))
        in
          apply (!root,b)
        end

    fun revfold abf (OS{root,...}) b =
          let fun apply (SplayNil, b) = b
                | apply (SplayObj{value,left,right},b) =
                    apply(right,abf(value,apply(left,b)))
        in
          apply (!root,b)
        end

    fun exists p (OS{root,...}) = let
          fun ex SplayNil = NONE
            | ex (SplayObj{value=v,left=l,right=r}) =
                if p v then SOME v
                else case ex l of
                       NONE => ex r
                     | a => a 
          in
            ex (!root)
          end

  end (* SplaySet *)
