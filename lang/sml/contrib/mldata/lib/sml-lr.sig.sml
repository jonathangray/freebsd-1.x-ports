

























signature LR =
sig
   datatype parseaction =
       ERROR
     | ACCEPT
     | SHIFT  of int
     | REDUCE of int * int * int
   and 'a ParseResult = ParseOK of 'a   |  ParseERR of 'a   |  ParseFAIL  ;

   val lr_out : outstream ref;

   val parse : (int * int->parseaction) ->
	       (int * int->int)         ->
	       (int * int->bool)        ->
	       int                      ->
	       int                      ->
	       (int->'11a list->'11a)   ->
	       string array             ->
	       ('11a->'b) ->(bool->unit) ->
	       (unit->int * '11a)       ->
	       bool                     ->      '11a ParseResult
end;

