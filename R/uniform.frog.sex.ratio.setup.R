#' Generate uniform frogs
#'
#' @param n integer number of frogs
#' @param ratio decimal (0-1) sex ratio (percentage of females)
#' @param pondsize numeric length of edge of a square pond (meters)
#' @param fixed logical whether you want fixed positions or not
#' @param seed.x numeric seed for x positions (only need if fixed == T)
#' @param seed.y numeric seed for y positions (only need if fixed == T)
#' @param buffer logical whether you want the frogs to be on the periphery or randomly distributed.
#' @return a dataframe of simulated frogs following the parameters you specify.
#' @export
#' @examples
#' uniform.frog.sex.ratio.setup(30, .5, 30, F, 300, 300)

uniform.frog.sex.ratio.setup <- function(n, ratio, pondsize, fixed, seed.x, seed.y, buffer) {
  # ratio is percentage of females
  # Sets up dataframe of simfrogs
  if(buffer == T) {
    buffer.set <- 3
    if(fixed == F) {
      weight <- (buffer.set - 0)/((buffer.set - 0) + (pondsize - (pondsize-buffer.set)))
      which_region <- rbinom(n, 1, prob = weight)
      x <- runif(n, ifelse(which_region, 0, (pondsize-buffer.set)), ifelse(which_region, buffer.set, pondsize))
      y <- runif(n, ifelse(which_region, 0, (pondsize-buffer.set)), ifelse(which_region, buffer.set, pondsize))
    } else {
      weight <- (buffer.set - 0)/((buffer.set - 0) + (pondsize - (pondsize-buffer.set)))
      which_region <- rbinom(n, 1, prob = weight)
      set.seed(seed.x)
      x <- runif(n, ifelse(which_region, 0, (pondsize-buffer.set)), ifelse(which_region, buffer.set, pondsize))
      set.seed(seed.y)
      y <- runif(n, ifelse(which_region, 0, (pondsize-buffer.set)), ifelse(which_region, buffer.set, pondsize))
    }
  } else {
    if(fixed == F) {
      x <- sample(c(0:pondsize), replace = T)
      y <- sample(c(0:pondsize), replace = T)
    } else {
      set.seed(seed.x)
      x <- sample(c(0:pondsize), replace = T)
      set.seed(seed.y)
      y <- sample(c(0:pondsize), replace = T)
    }
  }
  df <- as.data.frame(cbind(x,y))
  names(df) <- c("x", "y")
  df$id <- seq(1:n)
  df$sex <- sample(c("m", "f"), n, replace = T, prob = c(1-ratio, ratio))
  df
}


