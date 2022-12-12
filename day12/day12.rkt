#lang racket 

( require "../kck/kck.rkt" )
( require math/array )
( require racket/sequence )
( require predicates )
( require racket/control )
( require data/queue )

( require ( only-in algebraic/prelude id ) )

( define start-char #\S )
( define end-char #\E )

( define ( graph-parser cont ) 
  ( define lns ( lines cont ) )
  ( define rows ( length lns ) )
  ( define cols ( string-length ( car lns ) ) )
  ( define mat ( array-reshape ( list->array ( append-map string->list ( lines cont ) ) ) ( vector rows cols ) ) )
  ( define start ( car ( array-index-where ( eq?? start-char ) mat ) ) )
  ( define end ( car ( array-index-where ( eq?? end-char ) mat ) ) )
  ( define ( char->height v ) ( match v 
    [ #\S ( char-offset #\a #\a ) ] 
    [ #\E ( char-offset #\z #\a ) ] 
    [ o ( char-offset o #\a ) ] ) )
  ( define res-mat ( array-map char->height mat ) )
  ( list res-mat start end )
) 

( define ( neighbours graph pos ) 
  ( define height ( array-ref graph pos ) )
  ( filter ( lambda ( v ) 
              ( and 
                ( in-array-bounds? v graph )
                ( <= ( - ( array-ref graph v ) height ) 1 ) ) )
           ( 4-neigh pos ) ) ) 

( define ( shortest-path graph starts end ) 
  ( define visited ( array->mutable-array ( make-array ( array-shape graph ) 0 ) ) )
  ( for-each ( lambda ( v ) ( array-set! visited v 1 ) ) starts ) 
  ( define ( was-visited pos ) ( eq? ( array-ref visited pos ) 1 ) )
  ( define ( add-visited! pos ) ( array-set! visited pos 1 ) )

  ( define que ( apply queue ( map ( cons-on 0 ) starts ) ) )
  define pred ( equal?? end ) )

  ( define ( go ) 
    ( ( ind-pair ( lambda ( pos depth )
      ( if ( pred pos ) 
         ( shift k depth )
         ( for ( [ n ( neighbours graph pos ) ]
                                #:when ( not ( was-visited n ) ) )
                          ( add-visited! n ) 
                          ( enqueue! que ( cons n ( + depth 1 ) ) ) ) )
    ) ) ( if ( queue-empty? que ) ( shift k -1 ) ( dequeue! que ) ) )
  )  
  ( reset ( ad-infinitum ( thunk ( go ) ) ) )
)

( define ( part-1 cont ) ( match cont [ ( list graph start end )
  ( shortest-path graph ( list start ) end )
] ) )

( define ( part-2 cont ) ( match cont [ ( list graph start end )
  ( shortest-path graph ( array-index-where ( eq?? 0 ) graph ) end )
] ) )


( call-with-content "i.aoc" part-1 graph-parser )
( call-with-content "i.aoc" part-2 graph-parser )
