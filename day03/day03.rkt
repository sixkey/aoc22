                                  #lang racket

                          ( require "../kck/kck.rkt" )
                             ( require racket/set )
                             ( require algorithms )

                 ( define ( char->int c ) ( if ( char<? c #\a )
                         ( + ( char-offset c #\A ) 27 )
                       ( + ( char-offset c #\a ) 1 ) ) )

                         ( define ( parse-input line )
                   ( let ( [ length ( string-length line ) ]
                      [ linelist ( string->list line ) ] )
     ( bimap list->set list->set ( split-on linelist ( / length 2 ) ) ) ) )

     ( define ( parse-input-2 line ) ( list->set ( string->list line ) ) )

                            ( define ( part-1 mat )
                           ( sum ( map ( lambda ( p )
               ( char->int ( set-first ( set-intersect ( car p )
                               ( cdr p ) ) ) ) )
                                   mat ) ) )

                            ( define ( part-2 mat )
                          ( sum ( map ( lambda ( xs )
            ( char->int ( set-first ( apply set-intersect xs ) ) ) )
                           ( chunks-of mat 3 ) ) ) )

                 ( call-with-lines "i.aoc" part-1 parse-input )
                ( call-with-lines "i.aoc" part-2 parse-input-2 )
