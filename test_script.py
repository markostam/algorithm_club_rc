import unittest

def square(x):
  return x * x

class MyProgramTests(unittest.TestCase):
	def test_square(self):
	    self.assertEqual(16, square(4))
	    self.assertEqual(25, square(5))

if __name__ == '__main__':
    unittest.main()
