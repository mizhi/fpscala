import com.mizhi.fpscala._

// Exercise 3.2, p35
val l = List("foo", "bar", "baz")
List.tail(List.tail(l))

// Exercise 3.3, p36
List.setHead(l, "laff")

// Exercise 3.4, p36
List.drop(l, 1)

// Exercise 3.5, p36
val l2 = List(1,2,3,4,5,6,7,8)
List.dropWhile[Int](l2, x => x < 5)
List.dropWhile2[Int](l2, x => x < 5)
List.dropWhile3(l2)(x => x < 5)

// Exercise 3.6, p36
List.init(l2)

// Exercise 3.8, p40
List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_, _))
List.foldRight(List(1,2,3), Nil:List[Int])((a, b) => Cons(a, b))

// Exercise 3.9, p40
List.foldRight(List(1,2,3,4,5), 0)((a, b) => 1 + b)

// Exercise 3.11, p40
List.foldLeft(List(1,2,3), 0)(_ + _) // sum
List.foldLeft(List(1,2,3,4), 1)(_ * _) // product
List.foldLeft(List(1,2,3), 0)((a, b) => a + 1) // length

// Exercise 3.12, p41
List.reverse(List(1,2,3))

// Exercise 3.13, p41
List.foldRight2(List(1,2,3), Nil:List[Int])(Cons(_, _)) // Should be the identity

// Exercise 3.14, p41
List.append(List(1,2,3), List(4))

// Exercise 3.15, p41
List.concatAll(List(
  List(1,2,3),
  List(4,5,6),
  List(7,8,9)
))

// Exercise 3.16, p42
List.foldRight2(List(1,2,3), Nil:List[Int])((a, b) => Cons(a + 1, b))

// Exercise 3.17, p42
List.foldRight2(List(1.0,2.0,3.0), Nil:List[String])((a, b) => Cons(a.toString, b))
// Exercise 3.18, p42
List.map(List(1.0, 2.0, 3.0))(n => n.toString)
// Exercise 3.19, p42
List.filter(List(1,2,3,4,5))(_ < 4)

// Exercise 3.20, p42
List.flatMap(List(1,2,3))(i => List(i,i))

// Exercise 3.21, p43
List.filter2(List(1,2,3,4,5))(_ < 4)

// Exercise 3.22, p43
List.addLists(List(1,2,3), List(9,8,7))

// Exercise 3.23, p43
List.zipWith(List(1,2,3), List("2", "3", "4"))((a,b) => a + b.toInt)
// Exercise 3.24, p44
List.hasSubsequence(List(1,2,3,4,5), List(1,2,3))
List.hasSubsequence(List(1,2,3,4,5), List(3,2,1))
// Tree exercises starting on p46

// Exercise 3.25, p46
val t1 = Branch(
  Branch(
    Leaf("a"),
    Leaf("b")
  ),
  Branch(
    Leaf("c"),
    Leaf("d")
  )
)

Tree.size(t1)
