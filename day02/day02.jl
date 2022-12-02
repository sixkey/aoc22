                        function load_input( filename ) 
                            open( filename ) do file 
  map( map.( ( x -> x[ 1 ] ), split.( readlines( file ), " " ) ) ) do ( x, y )
                [ Int( x ) - Int( 'A' ), Int( y ) - Int( 'X' ) ]         
                                      end
                                      end
                                      end

                   posmod( x, y ) = x < 0 ? x % y + y : x % y
                              table = [ 3, 0, 6 ]

                             function part_1( mat ) 
                          sum( map( mat ) do ( x, y )
                    y + 1 + table[ posmod( x - y, 3 ) + 1 ] 
                                     end )
                                      end

                             function part_2( mat ) 
                         part_1( map( mat ) do ( x, y ) 
                         [ x, posmod( x - 1 + y, 3 ) ]
                                     end )
                                      end

           print( "part 1: ", part_1( load_input( "i.aoc" ) ), "\n" )
           print( "part 1: ", part_2( load_input( "i.aoc" ) ), "\n" )
