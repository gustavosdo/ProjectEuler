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

# Progress bar library
library(progress)

# Initial time of processing
t0 = Sys.time()

# Maximum number of n (in the example above, 13)
max_n = 1e6

# Length of chain: for 1 it is simply one, 
all_lengths = c(1, 2)

# Parallelism setup
library(doParallel)
library(parallel)
library(foreach)
cores = 63
cl = makeCluster(min(cores, detectCores() - 1), outfile = '/dev/null')
registerDoParallel(cl)

# Setting Progress Bar
pb <- progress::progress_bar$new(
  format = "Loop [:bar] :percent in :elapsed",
  total = (max_n - 2), clear = FALSE, width = 70)
progress_number = 3:max_n
progress = function(n){pb$tick(tokens = list(sp = progress_number[n]))}
opts <- list(progress = progress)

# Starting loop of n (3 to above)
all_lengths = foreach (n_start = 3:max_n, .packages = c('progress'),
                       .combine = 'c',.options.snow = opts) %dopar% {
  # progress bar call
  #pb$tick()
  # starting n
  n = n_start
  # Starting length of chain for integer n
  length_chain = 0
  # building up the chain from n_start to 1
  while (n != 1) {
    # checking parity
    if (((n %% 2) == 0)) {
      # checking if it is a power of 2
      if (log2(n) == trunc(log2(n))) {
        # adding power of 2 to length_chain
        length_chain = length_chain + log2(n)
        n = 1
      } else {
        # Applying collatz even number rule
        length_chain = length_chain + 1
        n = n/2
      }
    } else {
      # Applying collatz odd number rule
      length_chain = length_chain + 1
      n = 3*n + 1
    }
    # # Searching for previous results in order to speed-up the processing
    # if (n %in% 1:length(all_lengths)) {
    #   length_chain = length_chain + all_lengths[n]
    #   n = 1
    # }
  }
  # Adding total length to list of lengths
  return(length_chain)
}

# Printing results
message("Maximum index: ", max_n)
message('Solver time: ', format(Sys.time() - t0, digits = 2))
message("Maximum length index: ", (1:max_n)[all_lengths == max(all_lengths)])

# Quadratic fit (without parallelism) ------------------------------------------
# x = c(10, 100, 1000, 10000, 100000, 2e5, 2e4, 4e4, 6e4, 4e5)
# y = c(0.015, 0.016, 0.043, 0.39, 17, 132, 2.4, 6.2, 13, 2520)
# data = data.frame(x, y)
# fit = lm(formula = y ~ x + x^2, data = data)
# 
# new = data.frame(x = c(1e6))
# pred3 = predict(fit, new) # result: 1h30min