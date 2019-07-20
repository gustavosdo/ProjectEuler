# -*- coding: utf-8 -*-

'''
@author: gustavo.if.ufrj@gmail.com
'''

# * Problem description:
# The prime factors of 13195 are 5, 7, 13 and 29.
#
# What is the largest prime factor of the number 600851475143 ?

#number = 600851475143
number = 600000
primes = [1,2,3,5,7,9]
temp = []

for x in range(10, number):
	temp = [x%y for y in range(2,x)]
	if 0 in temp: pass
	else: primes.append(x)

print(primes)
