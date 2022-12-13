#lang racket

( require "../kck/kck.rkt" )

( require megaparsack )
( require megaparsack/text )
( require data/monad )
( require data/either )
( require data/applicative )
( require algorithms )
( require racket/control )
( require ( only-in algebraic/prelude id ) )
( require ( only-in predicates or? equal?? ) )
( require math/array )

( define ( parser cont )
  ( define number/p ( do ( ds <- ( many+/p digit/p ) )
                         ( pure ( string->number ( list->string ds ) ) ) ) )
  ( define ( line/p ) ( do ( char/p #\[ )
                           [ xs <- ( many/p ( or/p number/p ( line/p ) )
                                            #:sep ( char/p #\, ) ) ]
                           ( char/p #\] )
                           ( pure xs ) ) )
  ( define ( parse-line line )
    ( from-success #f ( parse-string ( line/p ) line ) )
  )
  ( pipe ( sliding ( lines cont ) 2 3 )
         ( curry map ( curry map parse-line ) ) )
)

( define ( comp a b )
  ( define ( term-comp a b )
    ( cond [ ( < a b ) ( shift k #t ) ]
           [ ( > a b ) ( shift k #f ) ]
           [ #t ] ) )
  ( define ( go . p ) ( match p
    [ ( list ( ? number? a ) ( ? number? b ) )
      ( term-comp a b ) ]
    [ ( list ( ? number? a ) ( ? list? b ) )
      ( go ( list a ) b ) ]
    [ ( list ( ? list? a ) ( ? number? b ) )
      ( go a ( list b ) ) ]
    [ ( list ( ? list? a ) ( ? list? b ) )
      ( zipwith go a b )
      ( term-comp ( length a ) ( length b ) )
    ] ) )
  ( reset ( go a b ) )
)

( define ( part-1 cont )
  ( sum ( map * ( map ( lambda ( v ) ( bool->number ( apply comp v ) ) ) cont )
                ( range-shift 1 0 ( length cont ) ) ) )
)

( define ( part-2 cont )
  ( define ( divider n ) ( list ( list n ) ) )
  ( define new-cont ( append ( list ( divider 2 ) ( divider 6 ) ) ( append-map id cont ) ) )
  ( define sorted ( sort new-cont comp ) )
  ( define divider? ( or? ( equal?? ( divider 2 ) ) ( equal?? ( divider 6 ) ) ) )
  ( prod ( filter pos? ( map * ( map ( compose bool->number divider? ) sorted )
                               ( range-shift 1 0 ( length sorted ) ) ) ) )
)

( call-with-content "i.aoc" part-1 parser )
( call-with-content "i.aoc" part-2 parser )
