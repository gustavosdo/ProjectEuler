#A palindromic number reads the same both ways. The largest palindrome
#made from the product of two 2-digit numbers is 9009 = 91 × 99.
#
#Find the largest palindrome made from the product of two 3-digit numbers.

rm(list=ls())
gc()

isPalindrome = function(N){
  L = nchar(N)
  return(as.logical(prod(sapply(1:L, function(x){substr(N, x, x) == substr(N, (L-x+1), (L-x+1))}))))
}

n = 999
m = 999
palindromes = c()
i = 0

for (n in 999:100)
{
  for (m in n:100)
  {
    flag = isPalindrome(as.character(m*n))
    if(flag) {palindromes[i] = m*n; i = i +1}
  }
}
print(max(palindromes))
