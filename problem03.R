# author: gustavo.if.ufrj@gmail.com

# * Problem description:
# The prime factors of 13195 are 5, 7, 13 and 29.
#
# What is the largest prime factor of the number 600851475143?

isPrime = function(x) prod(x %% 2:(x-1)) != 0

N = 600851475143
n = 29
flag = T

res = c()
i = 1

t0 = Sys.time()
while(flag){
  if (N %% n == 0)
  {
    if (isPrime(n))
    {
      res[i] = n
      i = i + 1
    }
  }
  n = n + 1
  
  if (prod(res) == N) flag = F
}
print(Sys.time() - t0)

print(res[length(res)])