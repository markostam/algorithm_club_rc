import breeze.linalg._

case class Node (parent: (Int,Int), coord: (Int,Int), g: Float, h: Float){
  val cost : Float = g+h
}

def nodeOrder (n: Node) = -n.cost

// object MinOrder extends Ordering[Int] {
//          def compare(x:Int, y:Int) = y compare x
//        }

val openList = scala.collection.mutable.PriorityQueue.empty(Ordering.by(nodeOrder))
var closedList : List[Node]= List()
val firstNode = Node((2,3),(2,3),0,0)


def l2 (x:(Any,Any),y:(Any,Any)) : Float = {
  // returns l2 or euclidean distance between two tuples
  def toFloat(x: Any) = x.asInstanceOf[Number].floatValue
  math.pow(List(x._1,x._2).zip(List(y._1,y._2)).
           map(x => math.pow(toFloat(x._1)-toFloat(x._2),2)).
           reduce(_+_),0.5).toFloat
}

def makeChildren (parent: Node, m: DenseMatrix[Float], goal: (Float,Float)) = {
  // gets neighbors of a point in a DenseMatrix
  val (r,c) = parent.coord
  m(r,c) = 1.0.toFloat
  val rowRange = (max((r-1), 0) to min((r+1), m.rows))
  val colRange = (max((c-1), 0) to min((c+1), m.cols))
  val sub = m(rowRange, colRange).toArray.toList
  val indices = rowRange.map(x => colRange.map(y => (x,y))).flatten.toList.
    zip(sub).filter(_._2 != 1.0).map(_._1)

  // convert those neighbors to child nodes 
  // with correctly populated parameters
  indices.
    map(newCoord => Node(parent.coord,newCoord,
    parent.g+l2(parent.coord,newCoord),l2(goal,newCoord)))
}


def aStar (openList : scala.collection.mutable.PriorityQueue, 
           closedList :List[Node], m: DenseMatrix[Float],
           start: (Int,Int), goal: (Int,Int) = {
  
  val q = openList.dequeue()
  val childern = makeChildren(q,m,goal)
  val testGoal = childern.filter(_.coord == goal)
  if (testGoal.isEmpty) {
    // stop search and return shortest path
  }
  else {
    
  }


}