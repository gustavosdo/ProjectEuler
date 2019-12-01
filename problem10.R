#The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
#
#Find the sum of all the primes below two million.

isPrime = function(x) prod(x %% 2:(x-1)) != 0

sum = 0
flag = F

t0 = Sys.time()
for (iter in 2:1999999){
  if (isPrime(iter)){
    sum = sum + iter
  }
}
Sys.time() - t0

sum
