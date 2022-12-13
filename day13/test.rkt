#lang racket

( require data/monad )
( require data/applicative )
( require megaparsack/text )

( sequence->list
  ( do [ x <- '( 1 2 3 ) ]
       ( pure ( * x 2 ) ) ) )
