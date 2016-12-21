//case class MaxHeap (heap : Vector[Int] = Vector(0))

case class MaxHeap (heapList : Vector[Int] = Vector(0)) {

  // constructor

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
    // returns indices of heap list to swap
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
    // and return a balanced heap
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
    // returns indices of heap list to swap
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
    (1 to k).par.toStream.foldLeft(heapList)(insert)
  }

  def populateBySeq (sequence : Seq[Int]) = {
    // populate the heap using a sequence
    sequence.foldLeft(heapList)(insert)
  }
}
