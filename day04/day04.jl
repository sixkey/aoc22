                       splitby( b ) = x -> split( x, b )
                    torange( x ) = range( x[1], stop=x[2] )
                           int = x -> parse( Int, x )
                     broadby( f ) = x -> broadcast( f, x )
                    tuple_of_list( l ) = ( l[ 1 ], l[ 2 ] )
                    ind_pair( f ) = p -> f( p[ 1 ], p[ 2 ] )

                     function on_input( solver, filename )
                             open( filename ) do f
eachline( f ) .|> ( x -> split( x, "," ) .|> splitby( "-" ) .|> broadby( int ) .|> torange ) .|>
                            tuple_of_list |> solver
                                      end
                                      end

                          function part_1( filename )
                        on_input( filename ) do content
content .|> ind_pair( ( a, b ) -> issubset( a, b ) || issubset( b, a ) ) |> sum
                                      end
                                      end

                          function part_2( filename )
                        on_input( filename ) do content
      content .|> ind_pair( intersect ) .|> isempty .|> ( x -> !x ) |> sum
                                      end
                                      end

                          println( part_1( "i.aoc" ) )
                          println( part_2( "i.aoc" ) )
