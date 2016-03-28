from concurrent.futures import ThreadPoolExecutor
from time import sleep

pool = ThreadPoolExecutor(8)

class Task():
	def __init__(self, gen):
		self.gen = gen
		self.step()

	def step(self, value=None):
		res = self.gen.send(value)
		res.add_done_callback(self._wakeup)

	def _wakeup(self, value):
		print('got: ', value.result())


def f(x,y):
	sleep(.1)
	return x+y

def do_func(x,y):
	yield(pool.submit(f, x, y))


Task(do_func(3,4))

