import breeze.linalg._

case class Astar (start : (Int,Int), goal : (Int,Int), 
                  m : DenseMatrix[Double] = DenseMatrix.zeros[Double](12,12)) {
  
  case class Node (parent: (Int,Int), coord: (Int,Int), g: Double, h: Double){
    val cost : Double = g+h
  }

  def l2 (x:(Any,Any),y:(Any,Any)) : Double = {
    // returns l2 or euclidean distance between two tuples
    def toDouble(x: Any) = x.asInstanceOf[Number].doubleValue
    math.pow(List(x._1,x._2).zip(List(y._1,y._2)).
             map(x => math.pow(toDouble(x._1)-toDouble(x._2),2)).
             reduce(_+_),0.5).toDouble
  }
  
  // fuction to order the heap by
  // we want a min heap of Nodes ordered by cost
  def nodeOrder (n: Node) = -n.cost
  val openList = scala.collection.mutable.PriorityQueue.empty(Ordering.by(nodeOrder))
  val closedList : List[Node] = List()
  val firstNode = Node(start,start,0,l2(start,goal))
  openList.enqueue(firstNode)

  def makeChildren (parent: Node, m: DenseMatrix[Double], 
                    goal: (Int,Int)) : List[Node] = {
    // get neighbors of a point in a DenseMatrix
    val (r,c) = parent.coord
    val rowRange = (max((r-1), 0) to min((r+1), m.rows-1))
    val colRange = (max((c-1), 0) to min((c+1), m.cols-1))
    val indices = rowRange.map(x => colRange.map(y => (x,y))).
      flatten.toList.filter(_ != parent.coord)
    // convert those matrix space neighbors to 
    // child nodes and populate parameters
    indices.
      map(newCoord => Node(parent.coord,newCoord,
      parent.g+l2(parent.coord,newCoord),l2(goal,newCoord)))
  }

  @annotation.tailrec
  final def createPath (node: Node, allVisited:List[Node], 
                  oldPath: List[(Double,Double)] = List()) : List[(Int,Int)] = {
    // 
    val newList : List[(Double,Double)] = List(node.coord).
      map(x => (x._1.toDouble,x._2.toDouble))
    val newPath = oldPath ++ newList
    if (node.g == 0.0) newPath.map(x => (x._1.toInt,x._2.toInt)).reverse
    else {
      // choose nextNode based on 
      //val dummyNode = Node((0,0),(0,0),Double.MaxValue,Double.MaxValue)
      val nextNode = allVisited.filter(_.coord == node.parent).head
        //foldLeft(dummyNode)((b:Node,a:Node) => if(b.g < a.g) b else a)
      createPath(nextNode,allVisited,newPath)
    }
  }

  @annotation.tailrec
  final def aStar (openList : scala.collection.mutable.PriorityQueue[Node], 
             closedList : List[Node], m: DenseMatrix[Double],
             start: (Int,Int), goal: (Int,Int)) : DenseMatrix[Double]= {
    val q = openList.dequeue()
    val children = makeChildren(q,m,goal)
    // if the goal is in the children, return shortest path
    if (children.exists(_.coord == goal)) {
      val goalNode = children.filter(_.coord == goal).head
      val allVisited = closedList ++ openList.toList ++ children ++ List(q)
      val shortestPath = createPath(goalNode,allVisited)
      allVisited.map(_.coord).foreach(x => m(x) = 8.8.toDouble)
      shortestPath.foreach(x => m(x) = 1.1.toDouble)
      m
    }
    // otherwise, enque nodes into open and close lists and continue search
    else {
      val testChildren = children.filterNot(x => openList.
        exists(y => x.coord == y.coord & x.cost > y.cost)).
        filterNot(x => closedList.
        exists(y => x.coord == y.coord & x.cost > y.cost))
      testChildren.foreach(x => openList.enqueue(x))
      val newClosedList : List[Node] = closedList ++ List(q)
      aStar(openList,newClosedList,m,start,goal)
    }
  }
  aStar(openList,closedList,m,start,goal)
}