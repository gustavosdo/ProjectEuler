# -*- coding: utf-8 -*-

'''
@author: gustavo.if.ufrj@gmail.com
'''

# * Problem description:
# Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be:
#
# 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
#
# By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.

fibonacci = [1,2]

n = 2
Sum = 2

while True:
	temp = fibonacci[n-2] + fibonacci[n-1]
	if ( temp > 4e6 ):
		break
	else:
		fibonacci.append(temp)
	n += 1
	if ( temp%2 == 0):
		Sum += temp

print(fibonacci)
print(Sum)
