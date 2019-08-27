package com.kugmax.learn.scala.datastructure

object Runner {
  def main(args: Array[String]): Unit = {

    System.out.println(List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))


//
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

    System.out.println( List.reverse( List(1, 2, 3) ) )
  }
}
