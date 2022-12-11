#lang racket

( require "../kck/kck.rkt" )

( require algorithms )
( require racket/struct )
( require ( only-in algebraic/prelude id ) )

( struct operation ( o n ) #:transparent )
( struct monkey ( op test t f ) #:transparent )

( define ( parse-input cont )
  ( define ( parse-line line ) ( match line
    [ ( pregexp "Starting items: ([^$]*)" ( list _ items ) )
        ( map string->number ( string-split items ", " ) ) ]
    [ ( pregexp "Operation: new = old ([+*]) ([\\d]+|old)" ( list _ op num ) )
        ( define pnum ( match num [ "old" num ] [ a ( string->number a ) ] ) )
        ( define pop ( match op [ "*" * ] [ "+" + ] ) )
        ( operation pop pnum ) ]
    [ ( pregexp "Test: divisible by ([\\d]+)" ( list _ num ) )
        ( string->number num ) ]
    [ ( pregexp "If [^:]*: throw to monkey ([\\d]+)" ( list _ monkey ) )
        ( string->number monkey ) ]
    )
  )
  ( define ( parse-monkey cont )
    ( bimap id ( lambda ( v ) ( apply monkey v ) ) ( map parse-line cont ) ) )
  ( bimap list->vector list->vector
          ( unzip-pair ( map parse-monkey
                             ( sliding ( map string-trim ( cdr ( lines cont ) ) )
                                       5 7 ) ) ) )
)

( define ( eval-op monkey-op v ) ( match monkey-op [ ( operation op num )
  ( op v ( match num [ "old" v ] [ n n ] ) )
] ) )

( define ( monkey-processor fun )
  ( lambda ( mon bag num-monkeys ) ( match mon [ ( monkey op test t f )
    ( define ( step v vec )
      ( define new-value ( fun ( eval-op op v ) ) )
      ( vector-update! vec
                       ( if ( divisible new-value test ) t f )
                       ( lambda ( v ) ( cons new-value v ) ) ) )
    ( foldl step ( make-vector num-monkeys ( list ) ) bag )
  ] ) )
)

( define ( get-system monkeys )
  ( foldl1 lcm ( map monkey-test ( vector->list monkeys ) ) )
)

( define ( solver monkey-proc cont steps ) ( match cont [ ( cons bags monkeys )
 ( define size ( vector-length monkeys ) )
 ( define res ( make-vector size ) )
 ( define ( step bags )
   ( for ( [ i ( in-range size ) ]
           [ m monkeys ] )
     ( define m-bag ( vector-ref bags i ) )
     ( vector-update! res i ( lambda ( v ) ( + v ( length m-bag ) ) ) )
     ( define dist ( monkey-proc m m-bag size ) )
     ( vector-map! append bags dist )
     ( vector-set! bags i ( list ) )
   )
   bags
 )
 ( church steps step bags )
 ( apply * ( take ( vector->list ( vector-sort res > ) ) 2 ) )
] ) )

( define ( part-1 cont )
  ( define monkey-proc
    ( monkey-processor ( lambda ( v ) ( quotient v 3 ) ) ) )
  ( solver monkey-proc cont 20 ) )

( define ( part-2 cont ) ( match cont [ ( cons bags monkeys )
  ( define ( monkey-proc system )
    ( monkey-processor ( lambda ( v ) ( modulo v system ) ) ) )
  ( solver ( monkey-proc ( get-system monkeys ) ) cont 10000 )
] ) )


( call-with-content "i.aoc" part-1 parse-input )
( call-with-content "i.aoc" part-2 parse-input )
