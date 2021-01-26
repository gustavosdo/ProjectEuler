# By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.
# 
#    3
#   7 4
#  2 4 6
# 8 5 9 3
# 
# That is, 3 + 7 + 4 + 9 = 23.
# 
# Find the maximum total from top to bottom of the triangle below:
#   
#                             75
#                           95 64
#                         17 47 82
#                       18 35 87 10
#                     20 04 82 47 65
#                   19 01 23 75 03 34
#                 88 02 77 73 07 63 67
#               99 65 04 28 06 16 70 92
#             41 41 26 56 83 40 80 70 33
#           41 48 72 33 47 32 37 16 94 29
#         53 71 44 65 25 43 91 52 97 51 14
#       70 11 33 28 77 73 17 78 39 68 17 57
#     91 71 52 38 17 14 91 43 58 50 27 29 48
#   63 66 04 68 89 53 67 30 73 16 69 87 40 31
# 04 62 98 27 23 09 70 98 73 93 38 53 60 04 23

triangle = list(c(75),
                c(95, 64),
                c(17, 47, 82),
                c(18, 35, 87, 10),
                c(20, 04, 82, 47, 65),
                c(19, 01, 23, 75, 03, 34),
                c(88, 02, 77, 73, 07, 63, 67),
                c(99, 65, 04, 28, 06, 16, 70, 92),
                c(41, 41, 26, 56, 83, 40, 80, 70, 33),
                c(41, 48, 72, 33, 47, 32, 37, 16, 94, 29),
                c(53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14),
                c(70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57),
                c(91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48),
                c(63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31),
                c(04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23))

triangle_test = list(c(75),
                     c(95, 64),
                     c(17, 47, 82),
                     c(18, 35, 87, 10))

triangle_test_2 = list(c(1),
                       c(0, 1),
                       c(0, 0, 1),
                       c(1, 0, 0, 0))

# Maximization algorithm -------------------------------------------------------
# I will try to solve this problem using a maximization algorithm. In order to
# do this I need to define the objective function and the bounds to be applied

# Objective function definition: -----------------------------------------------
# the objective function is defined mathematically by the sum of the elements of
# the original triangle multiplied by the weights to be choosen: w_ij is the
# weight (0 or 1) multiplying the element in the i-th row and j-th column of triangle.
model = list()
model$obj = unlist(triangle, recursive = T)

# Bounds -----------------------------------------------------------------------
# Binary response: each weight is either 0 or 1
model$vtype = 'B'

# Exclusivity: there is only one true boolean for each row of weights, i.e., the
# sum of all weights (per row) must be equal to 1
exc_start = rep(0, length(model$obj))
exc = c()
for (row in 1:length(triangle)) {
  exc = c(exc, c(rep(1, row), exc_start))
}
mat_exc = matrix(exc[1:(length(triangle)*length(exc_start))], ncol = length(model$obj), byrow = T)
# The right-hand-side for these lines are all 1 (rep(1, nrow(mat_exc)))
# To be added later in the rhs parameter of Linear Programming solver

# Path unity and neighborhood: the path must be chosen looking for neighbor cells
# Finding primary number and its neighbors
uni = c()
while(length(uni) < length(model$obj) - length(triangle)) {
  for (r in 1:(length(triangle) - 1)) {
    for (c in 1:length(triangle[[r]])) {
      # Reseting triangle
      uni_start = lapply(triangle, function(row){sapply(row, function(col){rep(0, length(col))})})
      # Primary number
      uni_start[[r]][c] = 1
      # Neighbours
      uni_start[[r+1]] = rep(1, length(uni_start[[r+1]]))
      uni_start[[r+1]][c] = 0
      uni_start[[r+1]][c+1] = 0
      # Returning result
      uni = c(uni, uni_start)
    }
  }
}
mat_uni = matrix(unlist(uni, recursive = T), ncol = length(model$obj), byrow = T)

# Global matrix
mat = rbind(mat_exc, mat_uni)

model$modelsense = "max"
model$A = mat
model$rhs = rep(1, nrow(mat))
model$sense = c(rep("=", nrow(mat_exc)), rep("<=", nrow(mat_uni)))

gurob = gurobi::gurobi(model = model, params = list(LogToConsole = 0))

symph = Rsymphony::Rsymphony_solve_LP(obj = model$obj,
                                      mat = model$A,
                                      dir = c(rep("==", nrow(mat_exc)), rep("<=", nrow(mat_uni))),
                                      rhs = model$rhs, types = "B",
                                      max = T)

print("Gurobi solution: "); print(gurob$x)
print("Rsymphony solution: "); print(symph$solution)


print("Gurobi max obj value: "); print(gurob$objval)
print("Rsymphony max obj value: "); print(symph$objval)
