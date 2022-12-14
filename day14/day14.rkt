#lang racket

( require "../kck/kck.rkt" )
( require racket/set )
( require math/array )
( require ( only-in algorithms sliding ) )
( require ( only-in racket/control shift reset ) )
( require ( only-in algebraic/prelude flip ) )
( require predicates )

( define ( parse-content content )
  ( define ( parse-line line )
    ( map ( lambda ( v ) ( list->vector ( map string->number ( string-split v "," ) ) ) )
          ( string-split line " -> " ) ) )
  ( define ( create-segment a b )
    ( foldl ( flip set-add ) ( set ) ( vec2-range-square a b ) ) )
  ( define ( create-path lst )
    ( apply set-union ( map ( curry apply create-segment ) ( sliding lst 2 ) ) )
  )
  ( apply set-union ( map ( compose create-path parse-line ) ( lines content ) ) )
)

( define ( next-position rocks sand )
  ( define ( not-in-rocks v ) ( not ( set-member? rocks v ) ) )
  ( cond/value [ ( pipe ( list #[ 0 1 ] #[ -1 1 ] #[ 1 1 ] )
                        ( curry map ( curry vector+ sand ) )
                        ( curry filter not-in-rocks ) ) not-null? car ]
               [ otherwise #f ] ) )

( define ( stepper-1 bottom rocks final sand solid )
  ( if ( ( vec2-y sand ) . > . bottom )
       ( final rocks )
       ( cond/value [ ( next-position rocks sand ) ]
                    [ otherwise ( solid sand ) ] )
) )

( define ( stepper-2 bottom rocks final sand solid )
  ( if ( ( vec2-y sand ) . = . ( + bottom 1 ) )
       ( solid sand )
       ( cond/value [ ( next-position rocks sand ) ]
                    [ otherwise ( if ( equal? sand #[ 500 0 ] )
                                     ( final rocks )
                                     ( solid sand ) ) ] )
) )

( define ( solver stepper rocks )
  ( define bottom ( apply max ( set-map rocks vec2-y ) ) )
  ( define ( sand-fall rocks final )
    ( set-add rocks
              ( call/cc ( church-infinity-with-cont
                          ( curry stepper bottom rocks final )
                          #[ 500 0 ] ) ) )
  )
  ( - ( set-count ( call/cc ( church-infinity-with-cont
                              sand-fall
                              rocks ) ) )
      ( set-count rocks ) )
)

( call-with-content "t.aoc" ( curry solver stepper-1 ) parse-content )
( + 1 ( call-with-content "t.aoc" ( curry solver stepper-2 ) parse-content ) )
