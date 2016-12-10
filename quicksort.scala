//functional quicksort algorithm in scala

class QuickSort {
	// quicksort function in scala
	def sort(a:Array[Int]) : Array[Int] = {
		if (a.length < 2) a 
		else {
			val pivot = a(a.length/2)
			sort(a.filter(_ < pivot)) ++ a.filter(_ == pivot) ++ sort(a.filter(_ > pivot))
			}
	}
}