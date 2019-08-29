package com.kugmax.learn.scala.datastructure

object Runner {
  def main(args: Array[String]): Unit = {

//    System.out.println(List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))
//    System.out.println(List.foldRight2(List(1,2,3), Nil:List[Int])(Cons(_,_)))

//    System.out.println( List.length( List(1, 1, 1, -1) ) )
//    System.out.println( List.length( List(1) ) )
//    System.out.println( List.length( List(5, 6) ) )
//    System.out.println( List.length( List() ) )
//

//    System.out.println( List.foldLeft( List(0, 1), 0)(_ + _) )

//    System.out.println( List.sum( List(0, 1, -1 , 5) ) )
//    System.out.println( List.product( List(2, 3) ) )

//      System.out.println( List.length( List(1, 1, 1, -1) ) )
//      System.out.println( List.length( List(1) ) )
//      System.out.println( List.length( List(5, 6) ) )
//      System.out.println( List.length( List() ) )

//    System.out.println( List.reverse( List(1, 2, 3) ) )
//    System.out.println( List.reverse( List(1, 2, 3, 4) ) )


//    System.out.println( List.append( List(1, 1, 1), List(2, 3, 4) ) )
//    System.out.println( List.append2( List(1, 1, 1), List(2, 3, 4) ) )
//    System.out.println( List.append3( List(1, 1, 1), List(2, 3, 4) ) )


//    System.out.println(List.foldRight(List(1,2,4,3,20), Nil:List[Int])(Cons(_,_)))
//    System.out.println( List.reduceList( List(List(1, 2), List(4), List(3, 20))  ))

//    System.out.println(List.map(List(1,2,3)) (a  => a + 10) )
//    System.out.println(List.map(List(1.0,2.0,3.0)) (a  => a.toString) )

//    System.out.println(List.filter(List(1,2,3)) (a  => a >= 2) )
//    System.out.println(List.filter2(List(1,2,3)) (a  => a >= 2) )

//    System.out.println(List(1,1,2,2,3,3) )
//    System.out.println(List.flatMap(List(1,2,3))(i => List(i, i)) )

//    System.out.println(List.elementSum(List(1,2,3), List(40, 50, 60) ) )
//    System.out.println(List.zipWith(List(1,2,3), List(40, 50, 60))(_+_)  )

    System.out.println(List.hasSubsequence(List(1, 2, 3, 4), List(2, 3)) )
    System.out.println(List.hasSubsequence(List(1, 2, 3, 4), List(2, 4)) )
    System.out.println(List.hasSubsequence(List(1, 2, 3, 4), List(4)) )
    System.out.println(List.hasSubsequence(List(1, 2, 3, 4), List(1, 2, 3, 4)) )

  }
}
