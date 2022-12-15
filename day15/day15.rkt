#lang racket

( require "../kck/kck.rkt" )
( require ( only-in algebraic/prelude flip ) )

( define ( parse-line line )
  ( define ( to-2vec2 a b c d ) ( list ( vector a b ) ( vector c d ) ) )
  ( match line
  [ ( pregexp "Sensor at x=([\\-\\d]+), y=([\\-\\d]+): closest beacon is at x=([\\-\\d]+), y=([\\-\\d]+)" ( list _ sx sy bx by ) )
    ( apply to-2vec2 ( map string->number ( list sx sy bx by ) ) ) ] ) )

( define ( get-cover line content )
  ( define ( man-dist u v ) ( + ( abs ( - ( vec2-x u ) ( vec2-x v ) ) )
                                ( abs ( - ( vec2-y u ) ( vec2-y v ) ) ) ) )
  ( define ( expand-sensor intervals sensor beacon )
    ( define dist ( man-dist sensor beacon ) )
    ( define y-dist ( abs ( - ( vec2-y sensor ) line ) ) )
    ( define ( inter-by-center c r ) ( interval ( - c r ) ( + c r ) ) )
    ( if ( y-dist . > . dist )
      intervals
      ( inter-set-add ( inter-by-center ( vec2-x sensor ) ( - dist y-dist ) ) intervals ) ) )
  ( foldl ( lambda ( sb tiles ) ( apply expand-sensor tiles sb ) )
          ( inter-set-empty ) content ) )

( define ( part-1 line content )
  ( define intervals ( get-cover line content ) )
  ( define beacons ( list->set ( map second content ) ) )
  ( - ( inter-set-cover intervals )
      ( length ( filter ( lambda ( b ) ( and ( ( vec2-y? line ) b )
                                             ( inter-set-contains ( vec2-x b ) intervals ) ) )
                        ( set->list beacons ) ) ) ) )

( define ( part-2 area content )
  ( define ( coord orientation )
    ( define ( covered-tiles-in line )
      ( inter-set-cover ( inter-set-intersection ( interval 0 area )
                                                 ( get-cover line orientation ) ) ) )
    ( car ( filter ( lambda ( v ) ( = ( covered-tiles-in v ) area ) )
                   ( range ( + area 1 ) ) )  ))
  ( define ( transpose-content cont )
    ( define ( vec2-flip v ) ( vector ( vec2-y v ) ( vec2-x v ) ) )
    ( map ( curry map vec2-flip ) cont ) )
  ( define y ( coord content ) )
  ( define x ( coord ( transpose-content content ) ) )
  ( + ( * x 4000000 ) y ) )

( call-with-lines "t.aoc" ( curry part-1 10 ) parse-line )
( call-with-lines "t.aoc" ( curry part-2 20 ) parse-line )
