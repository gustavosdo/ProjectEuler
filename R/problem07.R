# By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
#
# What is the 10001st prime number?

isPrime = function(x) prod(x %% 2:(x-1)) != 0

n = 13
m = 6

t0 = Sys.time()
while(m < 10002){
  if(isPrime(n)) {m = m + 1; print(n)}
  n = n + 1
}
print(n-1)
print(Sys.time() - t0)