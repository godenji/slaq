package slaq.ql

enum JoinType(val sqlName: String):
  case Inner extends JoinType("INNER")
  case Left extends JoinType("LEFT OUTER")
  case Right extends JoinType("RIGHT OUTER")
  case Outer extends JoinType("FULL OUTER")