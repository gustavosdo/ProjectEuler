# The following iterative sequence is defined for the set of positive integers:
#   
#   n → n/2 (n is even)
# n → 3n + 1 (n is odd)
# 
# Using the rule above and starting with 13, we generate the following sequence:
#   13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
# 
# It can be seen that this sequence (starting at 13 and finishing at 1) contains
# 10 terms. Although it has not been proved yet (Collatz Problem), it is thought
# that all starting numbers finish at 1.
# 
# Which starting number, under one million, produces the longest chain?
#   
# NOTE: Once the chain starts the terms are allowed to go above one million.

# Progress bar 
library(progress)

t0 = Sys.time()

max_n = 1e6

all_lengths = c(1, 2)

pb <- progress::progress_bar$new(
  format = "Loop [:bar] :percent in :elapsed",
  total = (max_n - 2), clear = FALSE, width= 70)

for (n_start in 3:max_n) {
  pb$tick()
  n = n_start
  length_chain = 0
  while (n != 1) {
    if (((n %% 2) == 0)) {
      if (log2(n) == trunc(log2(n))) {
        n = 1
        length_chain = length_chain + log2(n)
      } else {
        n = n/2
        length_chain = length_chain + 1
      }
    } else {
      n = 3*n + 1
      length_chain = length_chain + 1
    }
    if (n %in% 1:n_start) {
      n = 1
      length_chain = length_chain + all_lengths[n]
    }
  }
  all_lengths = c(all_lengths, length_chain)
}

message("Maximum index: ", max_n)

message('Solver time: ', format(Sys.time() - t0, digits = 2))

message("Maximum length index: ", (1:max_n)[all_lengths == max(all_lengths)])


# Quadratic fit ----------------------------------------------------------------
# x = c(10, 100, 1000, 10000, 100000, 2e5, 2e4, 4e4, 6e4)
# y = c(0.015, 0.016, 0.043, 0.39, 17, 132, 2.4, 6.2, 13)
# data = data.frame(x, y)
# fit = lm(formula = y ~ x + x^2, data = data)
# 
# new = data.frame(x = c(1e6))
# pred3 = predict(fit, new) # result: 2.9 min