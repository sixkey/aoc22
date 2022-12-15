#lang racket

( require racket/control )
( require predicates )
( require math/array )
( require ( only-in relation/function partial ) )
( require ( only-in algebraic/prelude id ) )
( require ( only-in data/queue make-queue enqueue! ) )
( require ( for-syntax racket/base ) )

;;;; IO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

( provide call-with-content call-with-lines )

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

( provide print-id trace traceln println* println-id println*-id )

( define ( print-id x )
    ( begin ( print x )
            x ) )

( define ( println-id x )
    ( begin ( println x )
            x ) )

( define ( println*-id . vs )
    ( begin ( apply println* vs )
            ( values vs ) ) )

( define ( trace a b ) ( begin ( print a ) b ) )
( define ( traceln a b ) ( begin ( println a ) b ) )

; /o\
( define ( println* . vs )
  ( for-each ( lambda ( v ) ( print v ) ( display " " ) ) vs )
  ( displayln "" ) )

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

( provide vector+ vector- vector* vector/ )

( define ( vector+ . vs ) ( apply vector-map + vs ) )
( define ( vector- . vs ) ( apply vector-map - vs ) )
( define ( vector* . vs ) ( apply vector-map * vs ) )
( define ( vector/ . vs ) ( apply vector-map / vs ) )

( provide vector-foldl vec2-x vec2-y vec2-x? vec2-y? vec2-range-square vec2-man-dist )

( define ( vec2-man-dist u v ) ( + ( abs ( - ( vec2-x u ) ( vec2-x v ) ) )
                                   ( abs ( - ( vec2-y u ) ( vec2-y v ) ) ) ) )

( define ( vector-foldl f i . vs )
  ( apply foldl f i ( map vector->list vs ) ) )

( define ( vec-bimap f g u ) ( interval ( f ( interval-a u ) )
                                        ( g ( interval-b u ) ) ) )

( define ( vec2-x v ) ( vector-ref v 0 ) )
( define ( vec2-y v ) ( vector-ref v 1 ) )
( define ( vec2-x? a ) ( lambda ( v ) ( = ( vec2-x v ) a ) ) )
( define ( vec2-y? a ) ( lambda ( v ) ( = ( vec2-y v ) a ) ) )

( define ( vec2-range-square a b )

  ( define xstart ( min ( vec2-x a ) ( vec2-x b ) ) )
  ( define xend   ( max ( vec2-x a ) ( vec2-x b ) ) )
  ( define ystart ( min ( vec2-y a ) ( vec2-y b ) ) )
  ( define yend   ( max ( vec2-y a ) ( vec2-y b ) ) )
  ( map list->vector ( cartesian-product
                       ( inclusive-range xstart xend )
                       ( inclusive-range ystart yend ) ) )
)

; This is not lazy, it will always go through all vectors
;( define ( vector-all pred . vs )
  ;( apply vector-foldl
    ;( lambda xs ( apply call-with-init-last ( lambda ( vs acc ) ( and ( pred vs ) acc ) ) ( list xs ) ) )
    ;#t vs ) )

;( define ( call-with-init-last f lst )
  ;( define ( splt lst ) ( match lst
    ;[ ( list el ) ( cons ( list ) el ) ]
    ;[ ( cons x xs ) ( bimap ( cons-by x ) id ( splt xs ) ) ]
  ;) )
  ;( ( ind-pair f ) ( splt lst ) )
;)

;( define ( nempty-foldr comb tlf lst ) ( match lst
  ;[ ( list el ) ( tlf el ) ]
  ;[ ( cons x xs ) ( comb x ( nempty-foldr comb tlf xs ) ) ]
;) )

;;;; Interval ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

( provide inter-set-empty interval interval-a interval-b inter-set-cover
          interval-intersect interval-touch interval-join inter-set-add
          inter-set-contains inter-set-intersection interval-intersection )

( struct inter-set ( intervals ) #:transparent )

( define ( inter-set-empty ) ( inter-set ( list ) ) )

( define ( interval a b ) ( vector a b ) )
( define interval-a vec2-x )
( define interval-b vec2-y )
( define ( interval-size u ) ( + ( abs ( - ( interval-b u ) ( interval-a u ) ) ) 1 ) )

( define ( inter-set-cover s )
  ( foldr ( lambda ( v i ) ( + ( interval-size v ) i ) )
          0 ( inter-set-intervals s ) ) )

( define ( interval-intersect u v )

  ;( println* "inter" u v )
  ( if ( > ( interval-a u ) ( interval-a v ) ) ( interval-intersect v u )
    ( >= ( interval-b u ) ( interval-a v ) )
  )
)

( define ( interval< u v ) ( < ( interval-a u ) ( interval-a v ) ) )
( define ( interval> u v ) ( > ( interval-a u ) ( interval-a v ) ) )

( define ( interval-touch u v )
  ( if ( interval> u v ) ( interval-touch v u )
    ( interval-intersect ( vec-bimap id ( curry + 1 ) u ) v ) ) )

( define ( interval-join u v )
  ( if ( interval> u v ) ( interval-join v u )
    ( if ( not ( interval-touch u v ) ) ( error "intervals don't touch" )
         ( interval ( min ( interval-a u ) ( interval-a v ) )
                    ( max ( interval-b u ) ( interval-b v ) ) ) ) ) )

( define ( interval-contains a i ) ( in-interval a ( interval-a i ) ( interval-b i ) ) )

( define ( inter-set-contains a s )
  ( for/or ( [ i ( inter-set-intervals s ) ] )
    ( interval-contains a i ) ) )

( define ( inter-set-add u s )

  ( define ( merge-step i l ) ( match l
    [ ( cons x xs )
        ( if ( interval-touch x i ) ( cons ( interval-join x i ) xs )
                                    ( cons i l ) ) ]
    [ ( list ) ( list i ) ]
  ) )
  ( define ( bubble-step l lst ) ( match lst
    [ ( cons r rst )
        ( cond [ ( interval-touch l r ) ( cons ( interval-join l r ) rst ) ]
               [ ( interval< r l ) ( cons r ( cons l rst ) ) ]
               [ else ( cons l ( cons r rst ) ) ] ) ]
  ) )
  ( inter-set ( pipe ( inter-set-intervals s )
                     ( curry foldr bubble-step ( list u ) )
                     ( curry foldr merge-step ( list ) )  ) )
)

( define ( interval-intersection u v )
  ( if ( interval> u v ) ( interval-intersection v u )
    ( if ( not ( interval-intersect u v ) ) #f
      ( interval ( max ( interval-a u ) ( interval-a v ) )
                 ( min ( interval-b u ) ( interval-b v ) ) )
    )
) )

( define ( inter-set-intersection i s )
  ( inter-set ( pipe ( inter-set-intervals s )
              ( curry filter ( curry interval-intersect i ) )
              ( curry map ( curry interval-intersection i ) ) ) ) )

;;;; Array ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


( provide 4-neigh in-array-bounds? array-index-where )

( define ( 4-neigh pos )
  ( map ( lambda ( v ) ( vector-map + v pos ) )
        ( list #[  1  0 ]
               #[  0  1 ]
               #[ -1  0 ]
               #[  0 -1 ] ) ) )

( define ( in-array-bounds? is ar )
  ( define shape ( array-shape ar ) )
  ( for/and ( [ s shape ] [ i is ] ) ( in-interval i 0 ( - s 1 ) ) )
)

( define ( array-index-where p ar )
  ( define ( step acc i ) ( let ( [ el ( array-ref ar i ) ] )
                                ( if ( p el ) ( cons i acc ) acc ) ) )
  ( sequence-fold step ( list ) ( in-array-indexes ( array-shape ar ) ) )
)

;;;; Queue ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

( provide queue )

( define ( queue . vs )
  ( define res ( make-queue ) )
  ( for-each ( lambda ( v ) ( enqueue! res v ) ) vs )
  res )

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

( provide unzip-pair extr-pair zipwith )

( define ( extr-pair f g ) ( lambda ( e ) ( cons ( f e ) ( g e ) ) ) )

( define ( unzip-pair lst )
  ( foldr ( lambda ( el acc ) ( cons ( cons ( car el ) ( car acc ) )
                                     ( cons ( cdr el ) ( cdr acc ) ) ) )
          ( cons ( list ) ( list ) )
          lst ) )

( define ( zipwith f . ls )
  ( if ( ( all? nonempty-list? ) ls )
       ( cons ( apply f ( map car ls ) ) ( apply zipwith f ( map cdr ls ) ) )
       ( list ) ) )

; Math

( provide minimum maximum bool->number prod )

( define ( minimum lst ) ( foldl1 min lst ) )

( define ( maximum lst ) ( foldl1 max lst ) )

( define ( bool->number n ) ( if n 1 0 ) )

( define ( prod lst ) ( foldl * 1 lst ) )

( provide pos? neg? )

( define ( pos? . vs ) ( ( all? ( >? 0 ) ) vs ) )

( define ( neg? . vs ) ( ( all? ( <? 0 ) ) vs ) )

; Misc

( provide build-mat cons-by cons-on filter-by )

( define ( build-mat r c f )
  ( build-list r ( lambda ( i ) ( build-list c ( lambda ( j ) ( f i j ) ) ) ) )
)

( define ( cons-by h ) ( λ ( t ) ( cons h t ) ) )

( define ( cons-on t ) ( lambda ( h ) ( cons h t ) ) )

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

( provide pipe cond/value otherwise )

( define ( pipe val . funs )
  ( foldl ( lambda ( f a ) ( f a ) ) val funs ) )

( define-syntax ( cond/value conds )
  ( syntax-case conds ( otherwise )
    [ ( _ ( otherwise v ) ) #'v ]
    [ ( _ ( v ) ) #'( when v v ) ]
    [ ( _ ( v p ) ) #'( when ( p v ) v ) ]
    [ ( _ ( v p f ) ) #'( when ( p v ) ( f v ) ) ]
    [ ( _ ( v ) c1 ... ) #'( if v v ( cond/value c1 ... ) ) ]
    [ ( _ ( v p ) c1 ... ) #'( if ( p v ) v ( cond/value c1 ... ) ) ]
    [ ( _ ( v p f ) c1 ... ) #'( if ( p v ) ( f v ) ( cond/value c1 ... ) ) ]
  )
)

( define-syntax otherwise
  ( lambda ( stx )
    ( raise-syntax-error #f "Illegal use of otherwise outside cond/value" stx ) ) )

;;;; Fun ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

( provide church church-infinity church-infinity-with-cont ad-infinitum )

( define ( church n f z )
  ( if ( = n 0 ) z ( church ( - n 1 ) f ( f z ) ) )
)

( define ( ad-infinitum f ) ( begin ( f ) ( ad-infinitum f ) ) )

( define ( church-infinity f z ) ( church-infinity f ( f z ) ) )

( define ( church-infinity-with-cont f z )
  ( lambda ( k ) ( ( church-infinity-with-cont f ( f z k ) ) k ) ) )

