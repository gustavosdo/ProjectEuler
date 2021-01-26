# 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
# 
# What is the sum of the digits of the number 2^1000?

num = sprintf("%.0f", 2^1000)
splt_num = as.numeric(strsplit(x = num, split = "")[[1]])
sum(splt_num)