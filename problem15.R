# Starting in the top left corner of a 2×2 grid, and only being able to move to
# the right and down, there are exactly 6 routes to the bottom right corner.
# How many such routes are there through a 20×20 grid?

grid_dim = 4 # Integer number to define the grid dimension
poss_mov = c('D', 'R') # Vector containing possible movements (Up, Down, Right, Left)
start_pt = c(0, 0) # Vector of starting position (x, y)

# Returns the new position after a movement m is applied to a point in (x, y)
walk = function(x, y, m) {
  if (m == 'D') {
    return(c(x, y - 1))
  }
  if (m == 'R') {
    return(c(x + 1, y))
  }
}

# Returns the sequence of steps in order to move the point from (x,y) to grid´s end
find_path = function(start_pt, grid_dim, poss_mov) {
  
  # Starting point
  x = start_pt[1]; y = start_pt[2]
  
  # String containing path
  route = NULL
  
  # Checking position
  while (x < grid_dim | y > -grid_dim) {
    # Choose a random step
    m = sample(x = poss_mov, size = 1)
    # Boundary conditions
    if (x == grid_dim) {m = 'D'}
    if (y == -grid_dim) {m = 'R'}
    # Walking
    new_pt = walk(x, y, m)
    x = new_pt[1]; y = new_pt[2]
    route = paste0(route, m)
  }
  
  return(route)
}

all_paths = c()
for (i in 1:1e3) {
  all_paths = c(all_paths, find_path(start_pt, grid_dim, poss_mov))
}

#print(unique(all_paths))
print(length(unique(all_paths)))

print(factorial(2*grid_dim)/(factorial(grid_dim)^2))