






signature extend =
sig
   structure array:
   sig
      val iter    : ('a->'b) ->'a array->unit;
      val lfold   : ('a * 'b->'a) ->'a->'b array->'a;
      val rfold   : ('b * 'a->'a) ->'a->'b array->'a;
      val arrayof : int * (int->'1a) ->'1a array
   end;

   structure integer:
   sig
      val iter  : (int->'a) ->(int * int) ->unit;
      val lfold : ('b * int->'b) ->'b->(int * int) ->'b;
      val rfold : (int * 'b->'b) ->'b->(int * int) ->'b
   end;

   structure ref:
   sig
      infix +:= -:= ::=;
      val +:= : int ref * int->unit;
      val -:= : int ref * int->unit;
      val ::= : 'a list ref * 'a->unit;
      val ++  : int ref->int;
      val --  : int ref->int
   end
end;


