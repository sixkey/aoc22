                                  #lang racket
                                   ( require 
                                "../kck/kck.rkt" 
                             algorithms ) ( define 
                                    ( solver 
                               window ) ( lambda 
                         ( content ) ( + ( index-where 
                        ( sliding ( string->list content 
                      ) window ) all-unique ) window ) ) )
                    ( call-with-content "i.aoc" ( solver 4 ) 
                                string-trim ) ( 
                               call-with-content 
                                "i.aoc" ( solver 
                               14 ) string-trim )
