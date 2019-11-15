# author: gustavo.if.ufrj@gmail.com

# * Problem description:
# The prime factors of 13195 are 5, 7, 13 and 29.
#
# What is the largest prime factor of the number 600851475143?

isPrime = function(x){
  if (x%%2 != 0 & x%%3 != 0 & x%%5 != 0 & x%%7 != 0){
    return(T)
  } else {
    return(F)
  }
}

N = 600851475143
n = N - 1
flag = T
###### NOT WORKING YEY
t0 = Sys.time()
while(flag){
  if (N%%n == 0 & isPrime(n)){
    print(n)
    flag = F
  } else {
    n = n - 1
  }
}
print(Sys.time() - t0)

###### NOT WORKING YEY
