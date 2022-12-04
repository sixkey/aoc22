#lang racket

( require parsack )
( require predicates )

( require "../kck/kck.rkt" )

( define ( parse line )
  ( list->pair
  ( map ( lambda ( l ) ( list->pair ( map string->number ( string-split l "-" ) ) ) )
        ( string-split line "," ) ) )
)

( define ( contains a b )
  ( and ( >= ( car b ) ( car a ) )
        ( <= ( cdr b ) ( cdr a ) ) ) )

( define ( overlap a b )
  ( if ( > ( car a ) ( car b ) )
       ( overlap b a )
       ( >= ( cdr a ) ( car b ) ) ) )

( define ( part-1 content )
    ( count ( ind-pair ( lambda ( a b )
                                ( or ( contains a b ) ( contains b a ) ) ) )
              content ) )

( define ( part-2 content )
    ( count ( ind-pair overlap ) content ) )

( call-with-lines "i.aoc" part-1 parse )
( call-with-lines "i.aoc" part-2 parse )
