#!/usr/bin/env python

from math import sin

def plot(f):
    for y in [f(x) for x in range(16)]:
        if y < 0:
            print(('*'*int((-y)*10)).rjust(10))
        else:
            print(' '*10+'*'*int(y*10))


#plot(lambda x: sin(.4*x))
plot(lambda x: 1.10**x)
#plot(lambda x: .08*x)
