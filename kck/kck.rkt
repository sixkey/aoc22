#lang racket

( require racket/control )

( require predicates )

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

( provide in-range divisible )

( define ( divisible a b ) ( = ( % a b ) 0 ) )

( define ( in-range a mn mx ) ( and ( <= a mx ) ( >= a mn ) ) )

;;;; Fun ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

( provide id )

( define ( id x ) x )

;;;; Pred ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

( define ( neq? a b ) ( not ( eq? a b ) ) )

;;;; List ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Cutting

( provide split-on slice ind-pair build-mat filter-by )

( define ( split-on lst pos ) ( cons ( take lst pos ) ( drop lst pos ) ) )

( define ( slice a b c ) ( take ( drop a b ) ( - c b ) ) )

; Inductions

( define ( ind-pair f ) ( lambda ( p ) ( f ( car p ) ( cdr p ) ) ) )

( define ( ind-list f ) ( lambda ( l ) ( map f l ) ) )

; Misc

( define ( build-mat r c f )
  ( build-list r ( lambda ( i ) ( build-list c ( lambda ( j ) ( f i j ) ) ) ) )
)

( define ( filter-by pred ) ( lambda ( lst ) ( filter pred lst ) ) )

( define ( map-index f lst ) ( map f ( range ( length lst ) ) lst ) )

( define ( all-unique lst )
  ( eq? ( check-duplicates lst ) #f )
)

;;;; String ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

( provide char-offset lines map-index all-unique )

( define ( char-offset ch1 ch2 ) ( - ( char->integer ch1 ) ( char->integer ch2 ) ) )

( define ( lines str ) ( string-split str "\n" ) )


