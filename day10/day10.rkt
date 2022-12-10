#lang racket

( require "../kck/kck.rkt" )
( require algorithms )

( struct addx ( num ) #:transparent )
( struct noop () #:transparent )

( define ( parse-line line )
  ( match line
    [ ( pregexp "addx ([-\\d]+)" ( list _ num ) ) ( addx ( string->number num ) ) ]
    [ ( pregexp "noop" ) ( noop ) ]
  )
)

( define ( fold-to-registers cmnds )
  ( define ( hack cmnd ) ( match cmnd
    [ ( noop )   ( list ( noop ) ) ]
    [ ( addx x ) ( list ( noop ) ( addx x ) ) ] ) )
  ( define ( step cmnd acc )
   ( cons ( + ( car acc ) ( match cmnd [ ( addx x ) x ]
                                       [ ( noop ) 0 ] ) ) acc ) )
  ( reverse ( foldl step ( list 1 ) ( append-map hack cmnds ) ) )
)

( define ( part-1 cmnds )
  ( define ( relevant-cycle cycle ) ( divisible ( - cycle 20 ) 40 ) )
  ( define registers ( fold-to-registers cmnds ) )
  ( sum ( map ( lambda ( i v ) ( if ( relevant-cycle i ) ( * v i ) 0 ) )
              ( range-shift 1 0 ( length registers ) )
              registers ) )
)

( define ( part-2 cmnds )
  ( define registers ( fold-to-registers cmnds ) )
  ( define ( step i r )
    ( if ( in-interval ( modulo i 40 ) r ( + r 2 ) ) #\# #\. )
  )
  ( map list->string
        ( chunks-of ( map step
                          ( range-shift 1 0 ( length registers ) )
                          registers )
                    40 ) )
)

( call-with-lines "i.aoc" part-1 parse-line )

( for-each println
  ( call-with-lines "i.aoc" part-2 parse-line ) )
