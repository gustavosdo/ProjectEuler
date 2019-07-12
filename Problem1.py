# -*- coding: utf-8 -*-

'''
@author: gustavo.if.ufrj@gmail.com
'''

# * Problem description:
# If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
# Find the sum of all the multiples of 3 or 5 below 1000.

Sum = 0

for n in range(0,100): # you can test with maximum = 20 (answer: 78)
	if ( n%3 == 0):
		Sum += n
		pass # in order to not repeat multiples of 3 AND 5 (like 15)
	elif ( n%5 == 0 ):
		Sum += n
		pass
	else:
		pass

print(Sum)
