structure MCSet = struct
    fun insert (a, nil) = [a]
      | insert (a:int, l as (hd::tl)) =
          if a < hd then a::l 
          else if a = hd then l 
          else hd::(insert(a, tl))
    
    fun union (nil, y) = y
      | union (x, nil) = x
      | union (set1 as ((hd1:int)::tl1), set2 as (hd2::tl2)) =
          if hd1 < hd2 then hd1::(union(tl1, set2))
          else if hd1 = hd2 then hd1::(union(tl1, tl2))
          else hd2::(union(set1, tl2))
    
    fun intersect (nil, y) = nil
      | intersect (x, nil) = nil
      | intersect (set1 as ((hd1:int)::tl1), set2 as (hd2::tl2)) =
          if hd1 < hd2 then intersect(tl1, set2)
          else if hd1 = hd2 then hd1::(intersect(tl1, tl2))
          else intersect(set1, tl2)
    
    fun setDifference (nil, y) = nil
      | setDifference (x, nil) = x
      | setDifference (set1 as ((hd1:int)::tl1), set2 as (hd2::tl2)) =
          if hd1 < hd2 then hd1::(setDifference(tl1, set2))
          else if hd1 = hd2 then (setDifference(tl1, tl2))
          else setDifference(set1, tl2)
    
    val emptyset = nil
    
    fun singleton x = [x] 

    fun isthere(a,nil) = false 
      | isthere(a,b::rest) = a = b orelse isthere(a,rest)
end

