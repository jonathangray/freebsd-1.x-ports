






















signature list =
sig
   nonfix All Some;
   infixr 7 <|;
   infixr 6 MAP;
   infix  5 </ //;
   infixr 5 \\ />;
   infix  4 ||| \ INTER;
   infixr 4 UNION;
   infix  0 nonmember member subset;

   val <|       : ('a->bool) * 'a list->'a list;
   val MAP      : ('a->'b) * 'a list->'b list;
   val \\       : ('a->'b) * ('a->bool) ->'a list->'b list;
   val </       : ('a * 'b->'b) * 'b->'a list->'b;
   val />       : 'a * ('a * 'b->'a) ->'b list->'a;
   val //       : ('a * 'a->'a) * 'a list->'a;          exception Reduce  ;
   val |||      : 'a list * 'b list->('a * 'b) list;    exception Zip  ;
   val first    : ('a->bool) ->'a list->'a;              exception First  ;
   val FIRST    : ('a->bool) ->'a list->'a option;
   val Some     : ('a->bool) ->'a list->bool;
   val All      : ('a->bool) ->'a list->bool;
   val member   : ''a * ''a list->bool;
   val subset   : ''a list * ''a list -> bool;
   val nonmember : ''a * ''a list->bool;
   val \        : ''a list * ''a list->''a list;
   val UNION    : 'a list * 'a list->'a list;
   val INTER    : ''a list * ''a list->''a list;
   val set      : ''a list->''a list
end;
