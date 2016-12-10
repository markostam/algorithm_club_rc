
class MaxHeap {
  // constructor
  val heapList : List[Int] = List(0)
  val currentSize : Int = 0

  // log base 2 function
  val log2 = (x: Double) => math.log10(x)/math.log10(2.0)

  // bubble up function
  // uses funky math to get where 
  def bubbleUpFunc(newSize :Int, doNotSwap: Int) = {
    val maxNumOfIdx = log2(newSize).floor.toInt
    val range = (0 to maxNumOfIdx)
    // func to convert nubmer of indexes to actual indexes
    val idxFunc = (i: Int, n: Int) => (math.pow(0.5,i)*n).floor.toInt
    val indices = range.map(x => idxFunc(x,newSize)).toList
    // drop indices that correspond to values > new value k
    val filtIndices = indices.filter(_ > doNotSwap)
    if (filtIndices.size < 2) List(List(0,0)) else filtIndices.sliding(2).toList
  }

  // swap values of two indices in a list given a tuple of their indices
  // helper for the insert function
  def swapIndicesFunc (heapList : List[Int], swapIdx: List[Int]) = {
    heapList.
      updated(swapIdx(0),heapList(swapIdx(1))).
      updated(swapIdx(1),heapList(swapIdx(0)))
  }

  def insert(k: Int, heapList: List[Int]) = {
    val tempList = heapList :+ k
    val newSize = heapList.size
    // get indices of values > new value k
    val doNotSwap = heapList.zipWithIndex.filter(x => x._1 > k).map(_._2).
      reverse.lift(0).getOrElse(0)
    val swapIdx = bubbleUpFunc(newSize, doNotSwap)
    swapIdx.foldLeft(tempList)(swapIndicesFunc) 
  }
}

/*
  def bubbleDown(j: int){
    var i = j
    while ((i * 2) <= currentSize){
      var mc = minChild(i)
      if (heapList(i) > heapList(mc)){
          var tmp = heapList(i)
          heapList.updated(i, heapList(mc))
          heapList.updated(mc, tmp)
        }
      i = mc
    }
  }

def minChild(i){}
  i.map(x => if (i * 2 + 1 > currentSize) i * 2)
    if (i * 2 + 1 > currentSize) i * 2
    else
        if self.heapList[i*2] < self.heapList[i*2+1]:
            return i * 2
        else:
            return i * 2 + 1


  def bubbleUp(j: Int){
    var i = j
    while (i/2 > 0){
      if (heapList(i) > heapList(i/2)){
        val tmp = heapList(i/2)
        heapList.updated((i/2), heapList(i))
        heapList.updated((i), tmp)
      i /= 2
      }
    }
  }

i = aList.size / 2

*/
