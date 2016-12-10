import unittest
from quicksort import quicksort

class MyProgramTests(unittest.TestCase):
	def test_reg(self):
	    self.assertEqual([3, 4, 5, 22, 22, 51, 343, 345], 
	    	   quicksort([51, 22, 3, 345, 22, 343, 4, 5]))
	def test_neg(self):
	    self.assertEqual([-2342,-3,0,123],quicksort([-2342,123,-3,0]))
	def test_empty(self):
	    self.assertEqual([],quicksort([]))
	def test_zeros(self):
		self.assertEqual([0],quicksort([0]))
	def test_string(self):
		self.assertEqual("you can't sort strings silly",quicksort([4,5.0,"foot"]))

if __name__ == '__main__':
	unittest.main()
