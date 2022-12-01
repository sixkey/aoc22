#lang racket

( define ( on-file filename f )
    ( f ( port->string ( open-input-file filename ) #:close? #t ) ) 
)

( define ( parse-input str ) 
    ( map ( lambda ( e ) ( map string->number ( string-split e "\n" ) ) ) ( string-split str "\n\n" ) )
) 

( define ( on-input filename f ) 
    ( on-file filename ( lambda ( c ) ( f ( parse-input c ) ) ) )
)

( define ( sum l ) ( foldr + 0 l ) )

( define ( part-1 cont ) 
    ( sum ( take ( sort ( map sum cont ) > ) 1 ) )
)

( define ( part-2 cont ) 
    ( sum ( take ( sort ( map sum cont ) > ) 3 ) )
)

( on-input "i.aoc" part-1 ) 
( on-input "i.aoc" part-2 ) 
