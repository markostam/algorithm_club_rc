import random
import timeit
import sys

# quicksort python

def quicksort(a):
	# test non number types
	try:
		a = [float(i) for i in a]
	except ValueError:
		return "you can't sort strings silly"
	# actual algorithm
	if len(a) < 2:
		return a
	else:
		pivot = a[int(len(a)/2)]
		return (quicksort(list(filter(lambda x: x < pivot, a))) +
					 list(filter(lambda x: x == pivot, a)) +
					 quicksort(list(filter(lambda x: x > pivot, a))))

def test_quicksort(quicksort):
	for power in range(1,7):
		maxx = 10**power
		a = [random.randint(0,maxx) for i in range(maxx)]
		start_time = timeit.default_timer()
		quicksort(a)
		t = timeit.default_timer() - start_time
		print('{:07.5f} for array size {}'.format(t,maxx))

if __name__ == '__main__':
	print(quicksort([i for i in sys.argv[1:]]))
