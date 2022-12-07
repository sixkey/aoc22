#lang racket

( require "../kck/kck.rkt" )
( require algorithms )

( struct cd ( folder ) )
( struct ls () )
( struct file ( size name ) )
( struct dir ( name ) )

( define ( parser line ) 
  ( match line 
    [ ( pregexp "\\$ cd ([\\S]+)" ( list _ folder ) ) ( cd folder ) ]
    [ ( pregexp "\\$ ls$" ) ( ls ) ]
    [ ( pregexp "dir ([a-z]+)" ( list _ name ) ) ( dir name ) ]
    [ ( pregexp "([\\d]+) ([^$]*)" ( list _ size name ) ) 
        ( file ( string->number size ) name ) ]
  ) 
)

( struct tree-file ( size ) )
( struct tree-dir ( nodes ) #:mutable ) 

( define ( update-nodes! f ) ( λ ( d ) 
    ( begin ( set-tree-dir-nodes! d ( f ( tree-dir-nodes d ) ) ) d ) ) )

( struct to-tree-st ( stack ) )   

( define ( to-tree lines ) 
  ( define st ( empty-stack ) )
  ( define ( add-node node ) 
    ( on-top! st ( update-nodes! ( cons-with node ) ) ) )
  ( define ( st-pop ) 
    ( let ( [ built ( pop! st ) ] ) ( add-node built ) ) )
  ( for ( [ line lines ] ) 
    ( match line
      [ ( file size _ ) ( add-node ( tree-file size ) ) ]
      [ ( cd ".." ) ( st-pop ) ]
      [ ( cd _ ) ( push! st ( tree-dir ( list ) ) ) ]
      [ _ ( void ) ]
    ) 
  )
  ( for ( [ i ( in-range ( - ( stack-length st ) 1 ) ) ] ) 
    ( st-pop ) )
  ( top st )
)

( define ( tree-kata on-dir on-file ) 
  ( define ( go node ) 
    ( match node 
      [ ( tree-dir xs ) ( on-dir ( map go xs ) ) ] 
      [ ( tree-file size ) ( on-file size ) ] )
  ) go
)

( define ( addp p q ) ( cons ( + ( car p ) ( car q ) )
                             ( + ( cdr p ) ( cdr q ) ) ) )

( define ( scan-tree tree ) 
  ( define res ( make-stack ) )
  ( ( tree-kata 
      ( λ ( xs ) ( let ( [ s ( sum xs ) ] ) ( push! res s ) s ) )
      id  
    ) tree )
  ( stack->list res )
)

( define ( part-1 commands ) 
  ( define tree ( to-tree commands ) )
  ( define lst ( scan-tree tree ) )
  ( sum ( filter ( λ ( v ) ( < v 100000 ) ) lst ) )
)

( define ( part-2 commands ) 
  ( define tree ( to-tree commands ) )
  ( define lst ( scan-tree tree ) )
  ( define total ( maximum lst ) )
  ( define ( enough v ) ( >= ( + ( - 70000000 total ) v ) 30000000 ) )
  ( minimum ( filter enough lst ) )
)

( call-with-lines "i.aoc" part-1 parser )
( call-with-lines "i.aoc" part-2 parser )
