#lang racket

( require "../kck/kck.rkt" )

( define ( char-offset ch1 ch2 ) ( - ( char->integer ch1 ) ( char->integer ch2 ) ) )

( define ( ch-off c ) ( lambda ( x ) ( char-offset ( string-ref x 0 ) c ) ) )

( define ( parse-input line )
    ( bimap ( ch-off #\A ) ( ch-off #\X )
        ( list->pair ( string-split line ) ) )
)

( define ( score a b ) ( * ( modulo ( - 1 ( - a b ) ) 3 ) 3 ) )

( define ( part-1 mat )
    ( sum ( map ( lambda ( p )
                ( + ( score ( car p ) ( cdr p ) )
                    ( cdr p )
                    1 ) )
                mat ) ) )

( define ( part-2 mat )
    ( part-1 ( map ( lambda ( p )
                       ( cons ( car p ) ( modulo ( - ( + ( car p ) ( cdr p ) ) 1 ) 3 ) ) )
                   mat ) ) )

( call-with-lines "i.aoc" part-1 parse-input )
( call-with-lines "i.aoc" part-2 parse-input )

