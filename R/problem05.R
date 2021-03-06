#2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
#
#What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

divisible = function(N) !as.logical(sum(sapply(1:20, function(x){ N %% x })))

N = 20
flag = T

t0 = Sys.time()
while(flag){
  flag = !divisible(N)
  N = N + 20
}
print(Sys.time() - t0)

print(N-20)
