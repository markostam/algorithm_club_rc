import breeze.linalg._

case class Node (parent: (Int,Int), coord: (Int,Int), g: Float, h: Float){
  val cost : Float = g+h
}

def nodeOrder (n: Node) = -n.cost

val openList = scala.collection.mutable.PriorityQueue.empty(Ordering.by(nodeOrder))
val closedList : List[Node]= List()
val goal = (10,9)
val start = (1,1)
val m = DenseMatrix.zeros[Float](20,20)

def l2 (x:(Any,Any),y:(Any,Any)) : Float = {
  // returns l2 or euclidean distance between two tuples
  def toFloat(x: Any) = x.asInstanceOf[Number].floatValue
  math.pow(List(x._1,x._2).zip(List(y._1,y._2)).
           map(x => math.pow(toFloat(x._1)-toFloat(x._2),2)).
           reduce(_+_),0.5).toFloat
}

val firstNode = Node(start,(2,3),0,l2(start,goal))
openList.enqueue(firstNode)

def makeChildren (parent: Node, m: DenseMatrix[Float], 
                  goal: (Int,Int)) : List[Node] = {
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

def createPath (node: Node, allVisited:List[Node], 
                oldPath: List[(Float,Float)] = List()) : List[(Float,Float)] = {
  val newList : List[(Float,Float)] = List(node.coord).
    map(x => (x._1.toFloat,x._2.toFloat))
  val newPath = oldPath ++ newList
  println("newPath: " + newPath)
  println("node: " + node)
  if (node.g == 0.0) newPath
  else {
    val nextNode = allVisited.filter(_.coord == node.parent)(0)
    println("nextNode: " + nextNode)
    createPath(nextNode,allVisited,newPath)
  }
}

def aStar (openList : scala.collection.mutable.PriorityQueue[Node], 
           closedList : List[Node], m: DenseMatrix[Float],
           start: (Int,Int), goal: (Int,Int)) : List[(Float,Float)]= {
  val q = openList.dequeue()
  val children = makeChildren(q,m,goal)
  if (children.exists(_.coord == goal)) {
    // return shortest path
    println("a* done :)")
    println("closedlist: " + closedList)
    val goalNode = children.filter(_.coord == goal).head
    println("goalNode: " + goalNode)
    val allVisited = closedList ++ openList.toList
    createPath(goalNode,allVisited)
  }
  else {
    println("iter")
    val testChildren = children.filterNot(x => openList.
      exists(y => x.coord == y.coord & x.cost > y.cost)).
      filterNot(x => closedList.
      exists(y => x.coord == y.coord & x.cost > y.cost))
    testChildren.foreach(x => openList.enqueue(x))
    val newClosedList : List[Node] = closedList ++ List(q)
    aStar(openList,newClosedList,m,start,goal)
  }
}
 