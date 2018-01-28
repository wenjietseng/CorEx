# This script port corex.py into r version

# functions can be found in other packages

# ramify: argmax

library(ramify) # ramify can only deal with 2D

argmax_3 <- function(arr, axis = 1) {
  if (length(dim(arr)) != 3) stop("input array doesn't have correct dimension")
  if (axis == 1) {
  	apply(arr, MARGIN = 1, which.max)
  }
  else if (axis == 2) {
  	apply(arr, MARGIN = 2, which.max)
  } 
  else {
  	apply(arr, MARGIN = 3, which.max)
  }
}




label <- function(p_y_given_x) {
  # Maximum likelihood labels for some distribution over y's
  return(argmax())
}