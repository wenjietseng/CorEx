# This script port corex.py into r version

# functions can be found in other packages

# ramify: argmax

# library(ramify) # ramify can only deal with 2D

init <- function(n_hidden = 2, dim_hidden = 2, batch_size = 1e6, max_iter = 400,
                 n_repeat = 1, eps = 1e-6, alpha_hyper = c(0.3, 1.0, 500.0),
                 balance = 0.0, missing_value = -1, seed = NULL, verbose = False) {
  dim_hidden <- dim_hidden  # Each hidden factor can take dim_hidden discrete values
  n_hidden <- n_hidden  # Number of hidden factors to use (Y_1,...Y_m) in paper
  missing_values <- missing_values  # Implies the value for this variable for this sample is unknown
  
  max_iter <- max_iter  # Maximum number of updates to run, regardless of convergence
  batch_size <- batch_size  # TODO: re-implement running with mini-batches
  n_repeat <- n_repeat  # TODO: Run multiple times and take solution with largest TC
  
  eps <- eps  # Change in TC to signal convergence
  lam <- alpha_hyper[1]  # Hyper-parameters for updating alpha
  tmin <- alpha_hyper[2]  # Hyper-parameters for updating alpha
  ttc <- alpha_hyper[3]  # Hyper-parameters for updating alpha
  
  balance <- balance # 0 implies no balance constraint. Values between 0 and 1 are valid.
  set.seed(seed)  # Set for deterministic results

  
  # change these following into R statements
  verbose <- verbose
  if (verbose > 0) {
    # print options only
    # np.set_printoptions(precision=3, suppress=True, linewidth=200) #
    # print('corex, rep size: {}, {}'.format(n_hidden, dim_hidden)) #
  }
    
  if (verbose > 1) {
    np.seterr(all='warn')
  }
  else {
    np.seterr(all='ignore')
  }
  # should return a list type, make these into an OO class later
}

argmax <- function(arr, axis = 0) {
  # Returns the indices of the maximum values along an axis.
  if (axis > length(dim(arr))) stop("axis doesn't fit the dimension of input array")
  # axis in numpy is different with R
  if (axis == 0) {
  	which.max(arr)
  } else {
  	apply(arr, MARGIN = axis, which.max)
  }
}


label.p.y.given.x <- function(p.y.given.x) {
  # Maximum likelihood labels for some distribution over y's
  t(argmax(p.y.given.x, axis = 3))
}

# @property in R?
ML.labels <- function() {
  # maximum likelihood labels for trainging data
  label.p.y.given.x(p.y.given.x)
}

# @property in R?
clusters <- function() {
  # return cluster labels for variables
  argmax(alpha[, , 2], axis = 2)
}

# @property in R?
tc <- function() {
  # The total correlation explained by all the Y's.
  # (Currently correct only for trees, modify for non-trees later.)
  sum(tcs)
}


event.from.sample <- function(x) {
  # Transform data into event format.
  # For each variable, for each possible value of dim_visible it could take (an event),
  # we return a boolean matrix of True/False if this event occurred in this sample, x.
  # Parameters:
  #   x: {array-like}, shape = [n_visible]
  # Returns:
  #   x_event: {array-like}, shape = [n_visible * self.dim_visible]
  x <- as.array(x)
  n.visible <- dim(x)[1] # number of columns
  stopifnot(self.n.visible == n.visible)
  # print "Incorrect dimensionality for samples to transform."

}
np.ravel(x[:, np.newaxis] == np.tile(np.arange(self.dim_visible), (n_visible, 1)))