#lang racket

( require "../kck/kck.rkt" )

( require predicates )

( define ( parse-crates r c lines ) 
  ( map ( filter-by ( not? ( eq?? #\space ) ) ) 
        ( build-mat c r ( lambda ( i j ) ( string-ref ( list-ref lines j ) ( + 1 ( * 4 i ) ) ) ) ) )
) 

( define ( parse-moves lines ) 
  ( map ( lambda ( line ) 
                 ( map string->number 
                       ( drop ( regexp-match #rx"move ([0-9]+) from ([0-9]+) to ([0-9]+)" line ) 1 ) ) ) 
        lines )
) 

( define ( create-parser r c ) 
  ( lambda ( content ) 
    ( cons ( parse-crates r c ( take ( lines content ) r ) )
           ( parse-moves ( drop ( lines content ) ( + r 2 ) ) ) )
  ) 
)

( define ( solver cargo-taker ) 
  ( lambda ( repr ) 
    ( define ( step move state )
      ( match move
        [ ( list num frm to ) 
          ( let ( [ cargo ( cargo-taker num ( list-ref state ( - frm 1 ) ) ) ] )
                ( map-index ( lambda ( i l ) 
                              ( cond 
                                [ ( = ( - frm 1 ) i ) ( drop l num ) ] 
                                [ ( = ( - to 1 ) i ) ( append cargo l ) ]
                                [ else l ] ) ) 
                            state )
          ) ]  
      )
    )
    ( list->string ( map car ( foldl step ( car repr ) ( cdr repr ) ) ) )
  )  

)

( define ( part-1 num lst ) ( reverse ( take lst num ) ) )
( define ( part-2 num lst ) ( take lst num ) )

( call-with-content "i.aoc" ( solver part-1 ) ( create-parser 8 9 ) )
( call-with-content "i.aoc" ( solver part-2 ) ( create-parser 8 9 ) )
