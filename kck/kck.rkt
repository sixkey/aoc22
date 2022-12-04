#lang racket

( require racket/control )

;;;; IO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

( provide call-with-content call-with-lines print-id trace )

( define ( read-lines file ) 
    ( begin ( define strport ( open-output-string ) )
            ( copy-port file strport )
            ( get-output-string strport ) ) )

( define ( call-with-content filename proc parse-content ) 
    ( call-with-input-file filename
      ( lambda ( file )
         ( proc ( parse-content ( read-lines file ) ) ) ) ) )

( define ( call-with-lines filename proc on-line ) 
    ( call-with-content filename proc 
        ( lambda ( c ) ( map on-line ( string-split c "\n" ) ) ) ) )

( define ( print-id x ) 
    ( begin ( print x ) 
            ( x ) ) )

( define ( trace a b ) ( begin ( print a ) b ) )

;;;; Cat ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

( provide bimap list->pair )

( define ( bimap f g p ) 
    ( cons ( f ( car p ) ) ( g ( cdr p ) ) ) 
)

( define ( list->pair l ) ( cons ( first l ) ( last l ) ) )

;;;; Math ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

( provide sum in-range divisible )

( define ( sum l ) ( foldr + 0 l ) )

( define ( divisible a b ) ( = ( % a b ) 0 ) )

( define ( in-range a mn mx ) ( and ( <= a mx ) ( >= a mn ) ) )

;;;; Fun ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

( provide id )

( define ( id x ) x )

;;;; List ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

( provide split-on slice ind-pair )

( define ( split-on lst pos ) ( cons ( take lst pos ) ( drop lst pos ) ) )

( define ( slice a b c ) ( take ( drop a b ) ( - c b ) ) )

( define ( ind-pair f ) ( lambda ( p ) ( f ( car p ) ( cdr p ) ) ) )

;;;; String ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

( provide char-offset slice-each )

( define ( char-offset ch1 ch2 ) ( - ( char->integer ch1 ) ( char->integer ch2 ) ) )

; quadratic /o\
( define ( slice-each lst size ) 
    ( build-list ( / ( length lst ) size ) ( lambda ( i ) 
        ( slice lst ( * i size ) ( * ( + i 1 ) size ) ) ) )
)
