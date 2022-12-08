#lang racket

( require "../kck/kck.rkt" )
( require math/matrix ) 
( require math/array ) 
( require algorithms )

( define ( parser c )
  ( list*->matrix ( map ( λ ( line ) ( map char->number ( string->list line ) ) ) ( lines c ) ) ) )

( define ( array-max ar ) ( array-all-fold ar max -1 ) )

( define ( matrix-indexed-map f mat ) 
  ( build-matrix ( matrix-num-rows mat ) 
                 ( matrix-num-cols mat ) 
                 ( lambda ( y x ) ( f y x ( matrix-ref mat y x ) ) ) ) )

( define ( mat-left mat y x ) 
  ( submatrix mat ( :: y ( + y 1 ) ) ( :: ( - x 1 ) -1 -1 ) ) )

( define ( mat-right mat y x ) 
  ( submatrix mat ( :: y ( + y 1 ) ) ( :: ( + x 1 ) ( matrix-num-cols mat ) ) ) )

( define ( mat-up mat y x ) 
  ( submatrix mat ( :: ( - y 1 ) -1 -1 ) ( :: x ( + x 1 ) ) ) )

( define ( mat-down mat y x ) 
  ( submatrix mat ( :: ( + y 1 ) ( matrix-num-rows mat ) ) ( :: x ( + x 1 ) ) ) )

( define ( mat-sides mat y x ) 
  ( list ( mat-up mat y x )
         ( mat-right mat y x )
         ( mat-down mat y x )
         ( mat-left mat y x ) ) )

( define ( part-1 mat ) 
  ( define ( part-1-step y x el ) 
    ( bool->number ( < ( minimum ( map array-max ( mat-sides mat y x ) ) ) el ) ) )
  ( array-all-fold ( matrix-indexed-map part-1-step mat ) + 0 )
)

( define ( part-2 mat ) 
  ( define ( sight ar el ) 
    ( match ( index-where ( array->list ar ) ( lambda ( v ) ( >= v el ) ) )
      [ #f ( array-size ar ) ]
      [ o ( + o 1 ) ] ) )
  ( define ( part-1-step y x el ) 
    ( prod ( map ( lambda ( v ) ( sight v el ) ) ( mat-sides mat y x ) ) ) )
  ( array-max ( matrix-indexed-map part-1-step mat ) )
)

( call-with-content "i.aoc" part-1 parser )
( call-with-content "i.aoc" part-2 parser )
