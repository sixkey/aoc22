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

( provide in-interval divisible range-shift )

( define ( divisible a b ) ( = ( modulo a b ) 0 ) )

( define ( in-interval a mn mx ) ( and ( <= a mx ) ( >= a mn ) ) )

( define range-shift ( lambda xs
  ( apply range ( map ( lambda ( v ) ( + v ( car xs ) ) ) ( cdr xs ) ) ) ) )

;;;; Pred ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

( define ( neq? a b ) ( not ( eq? a b ) ) )

;;;; Vector ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

( provide vector-update! )

( define ( vector-update! vec pos f ) 
  ( vector-set! vec pos ( f ( vector-ref vec pos ) ) )
  vec 
)

;;;; List ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Cutting

( provide split-on slice )

( define ( split-on lst pos ) ( cons ( take lst pos ) ( drop lst pos ) ) )

( define ( slice a b c ) ( take ( drop a b ) ( - c b ) ) )

; Inductions

( provide ind-pair ind-list diag foldl1 )

( define ( ind-pair f ) ( lambda ( p ) ( f ( car p ) ( cdr p ) ) ) )

( define ( ind-list f ) ( lambda ( l ) ( map f l ) ) )

( define ( diag x ) ( cons x x ) )

( define ( foldl1 f lst ) ( foldl f ( car lst ) ( cdr lst ) ) )

; Zip 

( provide unzip-pair extr-pair )

( define ( extr-pair f g ) ( lambda ( e ) ( cons ( f e ) ( g e ) ) ) )

( define ( unzip-pair lst ) 
  ( foldr ( lambda ( el acc ) ( cons ( cons ( car el ) ( car acc ) )
                                     ( cons ( cdr el ) ( cdr acc ) ) ) ) 
          ( cons ( list ) ( list ) ) 
          lst ) )

; Math

( provide minimum maximum bool->number prod )

( define ( minimum lst ) ( foldl1 min lst ) )

( define ( maximum lst ) ( foldl1 max lst ) )

( define ( bool->number n ) ( if n 1 0 ) )

( define ( prod lst ) ( foldl * 1 lst ) )

; Misc

( provide build-mat cons-with filter-by )

( define ( build-mat r c f )
  ( build-list r ( lambda ( i ) ( build-list c ( lambda ( j ) ( f i j ) ) ) ) )
)

( define ( cons-with a ) ( λ ( l ) ( cons a l ) ) )

( define ( filter-by pred ) ( lambda ( lst ) ( filter pred lst ) ) )

( define ( map-index f lst ) ( map f ( range ( length lst ) ) lst ) )

( define ( all-unique lst )
  ( eq? ( check-duplicates lst ) #f )
)

;;;; Stack ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

( provide empty-stack on-top! top bottom pop! push! stack->list make-stack
          stack-length stack )

( struct stack ( lst ) #:mutable #:transparent )

( define ( empty-stack ) ( stack ( list ) ) )

( define ( on-top! st f ) ( push! st ( f ( pop! st ) ) ) )
( define ( top st ) ( car ( stack-lst st ) ) )
( define ( bottom st ) ( cdr ( stack-lst st ) ) )
( define ( pop! st )
  ( let ( [ val ( top st ) ] ) ( set-stack-lst! st ( bottom st ) ) val ) )
( define ( push! st val ) ( set-stack-lst! st ( cons val ( stack-lst st ) ) ) )
( define make-stack ( λ vs ( stack vs ) ) )
( define ( stack->list st ) ( stack-lst st ) )
( define ( stack-length st ) ( length ( stack-lst st ) ) )

;;;; String ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

( provide char-offset lines map-index all-unique char->number )

( define ( char-offset ch1 ch2 ) ( - ( char->integer ch1 ) ( char->integer ch2 ) ) )

( define ( lines str ) ( string-split str "\n" ) )

( define ( char->number c ) ( - ( char->integer c ) ( char->integer #\0 ) ) )

( provide string-car )

( define ( string-car s ) ( string-ref s 0 ) )

;;;; Syntax ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

( provide pipe )

( define pipe
  ( lambda xs
    ( foldl ( lambda ( f a ) ( f a ) ) ( car xs ) ( cdr xs ) ) ) )

;;;; Fun ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

( provide church )
( define ( church n f z ) 
  ( if ( = n 0 ) z ( church ( - n 1 ) f ( f z ) ) )
)
