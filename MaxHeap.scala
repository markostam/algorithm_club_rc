
class MaxHeap {

  // constructor
  val heapList : Vector[Int] = Vector(0)


  def swapIndicesFunc (heapList : Vector[Int], swapIdx: Vector[Int]) : Vector[Int]= {
    // swap values of two indices in a list 
    // given a tuple of their indices
    // helper for the insert/delete functions
    heapList.
      updated(swapIdx(0),heapList(swapIdx(1))).
      updated(swapIdx(1),heapList(swapIdx(0)))
  }

  def bubbleUpFunc(heapList: Vector[Int], idx: Int, k: Int,
                     idxList : Vector[Int] = Vector()) : Vector[Int] = {
    // get indices of heap list to swap
    // when inserting k into the heap
    // by attaching k at tail and bubbling up
    val parentIdx = idx/2
    val parent = heapList(parentIdx)
    val newIdxList = idxList :+ idx
    if (parent < k & parentIdx != idx) {
      bubbleUpFunc(heapList,parentIdx,k,newIdxList)
    }
    else newIdxList
  }

  def insert(heapList: Vector[Int], k: Int) : Vector[Int] = {
    // insert k into correct location in a maxheap
    val tempList = heapList :+ k
    val idx = tempList.size - 1 
    val swapIdx = bubbleUpFunc(tempList, idx, k).sliding(2).toList.
      filterNot(x => x.contains(0))
    if (!swapIdx.isEmpty & swapIdx.flatten.size > 1){
      swapIdx.foldLeft(tempList)(swapIndicesFunc)
    }
    else tempList
  }

  def bubbleDownFunc(heapList: Vector[Int], idx: Int, 
                     idxList : Vector[Int] = Vector()) : Vector[Int] = {
    // get indices of heap list to swap
    // when deleting index idx from the heap
    // by checking for largest child and bubbling down
    val newIdxList = idxList :+ idx
    val childrenIdx = Vector(idx*2, idx*2 + 1)
    val children = childrenIdx.map(x => (x, heapList.lift(x))).filterNot(_._2 == None) // 
    if (!children.isEmpty) {
      val maxChild = children.maxBy(_._2)._1
      bubbleDownFunc(heapList,maxChild,newIdxList)
    }
    else newIdxList
  }

  def delete(heapList: Vector[Int], idx: Int) = {
    // delete value at idx from the maxheap
    // and return a balanced heap
    val swapIdx = bubbleDownFunc(heapList, idx).sliding(2).toList.
      map(x => if (x.size < 2) Vector(x(0),x(0)) else x)
    val finalDeleteIdx = swapIdx.flatten.last
    swapIdx.foldLeft(heapList)(swapIndicesFunc).
      zipWithIndex.filter(_._2 != finalDeleteIdx).map(_._1)
  }

  def populateByMax (k : Int) = {
    // populate the heap using a range from 1 to k
    (1 to k).foldLeft(heapList)(insert)
  }

  def populateBySeq (sequence : Seq[Int]) = {
    // populate the heap using a sequence
    sequence.foldLeft(heapList)(insert)
  }
}

/*

old functional functions

  val currentSize : Int = 0

  // log base 2 function
  val log2 = (x: Double) => math.log10(x)/math.log10(2.0)

  // bubble up function
  // uses idxFunc to get indices that will need to be swapped 
  def bubbleUpFunc(newSize :Int, doNotSwap: Int) : Vector[Vector[Int]] = {
    val maxNumOfIdx = log2(newSize).floor.toInt
    val range = (0 to maxNumOfIdx)
    // func to convert range of indices to actual indices
    val idxFunc = (i: Int, n: Int) => (math.pow(0.5,i)*n).floor.toInt
    val indices = range.map(x => idxFunc(x,newSize)).toList
    // drop indices that correspond to values > new value k
    val filtIndices = indices.filter(_ > doNotSwap)
    if (filtIndices.size < 2) Vector(Vector(0,0)) 
      else filtIndices.sliding(2).toVector.map(_.toVector)
  }

  def insert(heapList: Vector[Int], k: Int) : Vector[Int] = {
    val tempList = heapList :+ k
    val newSize = heapList.size
    // get indices of values > new value k
    val doNotSwap = heapList.zipWithIndex.filter(x => x._1 > k).map(_._2).
      reverse.lift(0).getOrElse(0)
    // tuples of indices to swap
    val swapIdx = bubbleUpFunc(newSize, doNotSwap) 
    swapIdx.foldLeft(tempList)(swapIndicesFunc) 
  }


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
