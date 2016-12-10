from bitarray import bitarray

class BloomFilter:
	'''
	creates a bloom filter
	'''

	def __init__(self):
		# use bitarray instead of list to save space
		self.ba = bitarray(10**14)

	# TODO: these hashes don't work for large vocabularies
	#       use a murmur hash instead
	def _hash_fxn1(self, word):
		y=1
		if type(word) == str:
			for let in word:
				b = format(ord(let), 'b')
				y_gen = (i[1] for i in enumerate(b) if i[0]%2 == 0)
				y *= int(''.join(y_gen),2)
		else:
			b = format(word, 'b')
			y_gen = (i[1] for i in enumerate(b) if i[0]%2 == 0)
			y *= int(''.join(y_gen),2)
		return y%11

	def _hash_fxn2(self, word):
		y=1
		if type(word) == str:
			for let in word:
				b = format(ord(let), 'b')
				y_gen = (i[1] for i in enumerate(b) if i[0]%2 != 0)
				y *= int(''.join(y_gen),2)
		else:
			b = format(word, 'b')
			y_gen = (i[1] for i in enumerate(b) if i[0]%2 != 0)
			y *= int(''.join(y_gen),2)
		return y%11

	def write(self, x):
		self.ba[self._hash_fxn1(x)] = 1
		self.ba[self._hash_fxn2(x)] = 1

	def check(self, word):
		total = int(self.ba[self._hash_fxn2(word)]) + int(self.ba[self._hash_fxn1(word)])
		if total/2 == 1:
			print("Item is in the bloom filter")
		else:
			print("Item is not in the bloom filter")

	def read_in_dict(self):
		'''
		reads in local dict
		'''
		try:
			with open("/usr/share/dict/words") as f:
				words = f.readlines()
				words = [i.strip('\n') for i in words]
		except:
			print("no dictionary found")
		for word in words:
			self.write(word)
