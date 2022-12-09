#lang racket

( require "../kck/kck.rkt" )
( require algorithms )
( require ( only-in algebraic/prelude flip id ) )
( require math/array )


( define ( parse-line line ) 
  ( bimap string-car string->number ( list->pair ( string-split line " " ) ) ) )

( define ( vec2 x y ) ( array #[ x y ] ) )

( define ( dir->vec2 c )
  ( match c [ #\R ( vec2  0  1 ) ]
            [ #\L ( vec2  0 -1 ) ]
            [ #\U ( vec2  1  0 ) ]
            [ #\D ( vec2 -1  0 ) ] ) 
)

( define ( tail-to-head tail head ) 
  ( define diff ( array- head tail ) )
  ( define max-diff ( array-all-fold ( array-abs diff ) max ) )
  ( if ( <= max-diff 1 ) 
       tail
       ( array- head ( array-map ( lambda ( v ) 
                                   ( if ( eq? ( abs v ) max-diff ) 
                                        ( sgn v ) 0 ) )
                                        diff ) ) )
)

( struct state ( rope visited ) #:transparent )

( define ( make-state n ) ( state ( build-list n ( const ( vec2 0 0 ) ) ) ( list ) ) )

( define ( solver n ) 
  ( define ( step dir st ) 
    ( define ( link el acc ) 
      ( cons ( if ( empty? acc ) 
                  ( array+ el ( dir->vec2 dir ) )
                  ( tail-to-head el ( car acc ) ) ) acc ) )
    ( define new-rope ( foldr link ( list ) ( state-rope st ) ) )
    ( define new-visited ( cons ( car new-rope ) ( state-visited st ) ) )
    ( state new-rope new-visited )
  )
  ( lambda ( lines )
    ( define final-state ( foldl step ( make-state n ) 
             ( append-map ( ind-pair ( flip repeat ) ) lines ) ) )
    ( length ( remove-duplicates ( state-visited final-state ) ) )
  )
)

( call-with-lines "i.aoc" ( solver 2 ) parse-line )
( call-with-lines "i.aoc" ( solver 10 ) parse-line )
