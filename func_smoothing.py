from time import time, sleep

def smooth(avg, n, factor=.5):
	return (n * factor) + ((1 - factor) * avg)

def smooth_stream(stream, avg=0, prev=0):
	x = next(stream)
	yield avg
	for i in smooth_stream(stream, smooth(avg, x), x):
		yield i

stream = (10  for _ in range(10))
for i in smooth_stream(stream):
	print(i)

