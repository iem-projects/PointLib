// Type Scala code here.
// Press 'Shift + Enter' to execute selected text
// or current line.

// from svm_kreuz_brute4.txt
val x = Vector(
  Vector(3, 20, 26) -> 55.17,
  Vector(7, 14, 24) -> 55.17,
  Vector(11, 18, 20) -> 58.62,
  Vector(8, 10, 18) -> 62.07,
  Vector(3, 4, 14) -> 64.71,
  Vector(11, 19, 20, 26) -> 65.52,
  Vector(15, 18, 20, 21) -> 65.52,
  Vector(7, 18, 20, 21) -> 65.52,
  Vector(0, 3, 14, 26) -> 65.52,
  Vector(16, 18, 20, 27) -> 65.52,
  Vector(3, 7, 9, 14) -> 68.97,
  Vector(3, 4, 9, 14) -> 72.42,
  Vector(13, 14, 17, 19, 21) -> 75.86,
  Vector(0, 14, 16, 18, 19) -> 75.86,
  Vector(3, 14, 22, 26, 27) -> 75.86,
  Vector(2, 14, 16, 18, 19, 21) -> 76.47
)

val y = x.flatMap { case (v, w) => v.map(_ -> w) }

val m = (Map.empty[Int, Double].withDefaultValue(0.0) /: y) { case (m0, (i, w)) =>
  m0 + (i -> (m0(i) + w))
}

val z = m.toVector.sortBy(-_._2)
z.size
val v = z.take(6).map(_._1).sorted  // Vector(3, 14, 18, 19, 20, 21)

v.plot()

// next steps
// - try kreuz again without classes (full octave support)
// - horiz 'vertical-pairs' didn't yield much
// - instead use simple vector

OtherFeatures ; split-test
-t 2 -c 100 -w 0 1.00 -w 1 2.91 -g 1

// from svm_horiz1.txt
val x = Vector(
Vector(35, 43) -> 58.8235,
Vector(0, 1) -> 58.82353,
Vector(15, 17) -> 64.70588,
Vector(10, 14) -> 68.965515,
Vector(14, 17) -> 68.965515,
Vector(15, 32) -> 70.588234,
Vector(15, 37) -> 70.588234,
Vector(15, 44) -> 70.588234,
Vector(23, 32) -> 70.588234,
Vector(13, 15, 43) -> 75.86207,
Vector(0, 15, 47) -> 76.47059,
Vector(10, 23, 32) -> 79.31035,
Vector(12, 23, 40) -> 79.31035,
Vector(1, 9, 44) -> 79.31035,
Vector(13, 39, 43) -> 82.35294,
Vector(12, 23, 24, 40) -> 82.75862,
Vector(1, 4, 15, 35) -> 86.206894,
Vector(10, 13, 14, 44) -> 86.206894,
Vector(2, 10, 17, 20) -> 86.206894,
Vector(10, 20, 42, 47) -> 86.206894,
Vector(1, 23, 35, 40) -> 86.206894,
Vector(10, 11, 16, 20) -> 86.206894,
Vector(10, 14, 16, 28, 44) -> 88.23529,
Vector(13, 14, 27, 35, 44) -> 88.23529,
Vector(13, 14, 27, 41, 44) -> 88.23529,
Vector(13, 14, 28, 33, 46) -> 88.23529,
Vector(13, 14, 31, 35, 39) -> 88.23529,
Vector(13, 14, 35, 36, 43) -> 88.23529,
Vector(10, 14, 20, 40, 44) -> 88.23529,
Vector(12, 14, 15, 32, 44) -> 88.23529,
Vector(13, 15, 23, 37, 43) -> 88.23529,
Vector(13, 15, 23, 42, 43) -> 88.23529,
Vector(0, 1, 14, 32, 39) -> 88.23529,
Vector(13, 17, 31, 35, 41) -> 88.23529,
Vector(8, 13, 15, 23, 43) -> 88.23529,
Vector(9, 10, 14, 27, 47) -> 88.23529,
Vector(7, 9, 23, 39, 40) -> 88.23529,
Vector(0, 2, 8, 13, 33) -> 88.23529,
Vector(13, 19, 32, 36, 39) -> 88.23529,
Vector(6, 8, 13, 14, 45) -> 88.23529,
Vector(1, 13, 15, 42, 43) -> 88.23529,
Vector(14, 28, 29, 39, 40) -> 88.23529,
Vector(1, 13, 19, 24, 41) -> 88.23529,
Vector(0, 14, 19, 32, 36) -> 88.23529,
Vector(10, 11, 14, 16, 44) -> 88.23529,
Vector(5, 10, 13, 14, 44) -> 88.23529,
Vector(2, 10, 13, 14, 44) -> 88.23529,
Vector(7, 14, 15, 32, 37) -> 88.23529,
Vector(10, 13, 14, 40, 44) -> 88.23529,
Vector(10, 13, 14, 40, 46) -> 88.23529,
Vector(10, 13, 15, 33, 47) -> 93.10345
)

val y = x.flatMap { case (v, w) => v.map(_ -> w) }

val m = (Map.empty[Int, Double].withDefaultValue(0.0) /: y) { case (m0, (i, w)) =>
  m0 + (i -> (m0(i) + w))
}

val z = m.toVector.sortBy(-_._2)
z.size
val v = z.take(6).map(_._1).sorted  // Vector(10, 13, 14, 15, 23, 44)

//////////////////////
// so there are two options, the "weighted" best results

// combi1
Vector(3, 14, 18, 19, 20, 21)  // for vertical
Vector(10, 13, 14, 15, 23, 44) // for horizontal

// ... or the absolute best results

// combi2
Vector(2, 14, 16, 18, 19, 21)  // for vertical
Vector(10, 13, 15, 33, 47)     // for horizontal

//////////////////////////////////////////////////
// results for combi1 (-g 1 and split)
Vector(5, 8)
32 out of 46 (64.70588%)
Vector(0, 9)
32 out of 46 (65.51724%)
Vector(9, 11)
36 out of 46 (70.588234%)
Vector(0, 6, 8)
34 out of 46 (72.413795%)
Vector(0, 8, 9)
38 out of 46 (82.35294%)
Vector(6, 7, 8, 11)
40 out of 46 (86.206894%)
Vector(0, 4, 6, 7, 8, 9, 11)
41 out of 46 (88.23529%)
Vector(1, 4, 6, 7, 8, 9, 11)
42 out of 46 (88.23529%)
---RESULT---
Vector(0, 4, 6, 7, 8, 9, 11)
41 out of 46 (88.23529%)

////////////////////////////////////////
// results for combi2 (-g 1 and split)
Vector(6, 8)
29 out of 46 (58.62069%)
Vector(1, 8)
29 out of 46 (58.62069%)
Vector(6, 8, 10)
34 out of 46 (65.51724%)
Vector(7, 8, 10)
32 out of 46 (68.965515%)
Vector(0, 6, 8, 10)
35 out of 46 (75.86207%)
Vector(6, 7, 8, 9)
38 out of 46 (82.35294%)
Vector(6, 7, 8, 9, 10)
43 out of 46 (93.10345%)
---RESULT---
Vector(6, 7, 8, 9, 10)
43 out of 46 (93.10345%)

