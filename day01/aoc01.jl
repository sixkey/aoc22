                        function load_input( filename )
                                   sums = []
                            open( filename ) do file
                                    loc = []
                         for line in readlines( file )
                                 if line == ""
                               push!( sums, loc )
                                    loc = []
                                      else
                        push!( loc, parse( Int, line ) )
                                      end
                                      end
                                      end
                                  return sums
                                      end

                     part1( vec ) = maximum( sum.( vec ) )
       part2( vec ) = sum( first( sort( sum.( vec ), rev = true ), 3 ) )

                          vec = load_input( "i.aoc" )
                     print( "part1: ", part1( vec ), "\n" )
                     print( "part2: ", part2( vec ), "\n" )
