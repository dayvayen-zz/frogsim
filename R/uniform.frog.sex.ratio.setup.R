#' Generate uniform frogs
#'
#' @param n integer number of frogs
#' @param ratio decimal (0-1) sex ratio (percentage of females)
#' @param pondsize numeric length of edge of a square pond (meters)
#' @param fixed logical whether you want fixed positions or not
#' @param seed.x numeric seed for x positions (only need if fixed == T)
#' @param seed.y numeric seed for y positions (only need if fixed == T)
#' @return a dataframe of simulated frogs following the parameters you specify.
#' @export
#' @examples
#' uniform.frog.sex.ratio.setup(30, .5, 30, F, 300, 300)

uniform.frog.sex.ratio.setup <- function(n, ratio, pondsize, fixed, seed.x, seed.y) {
  # ratio is percentage of females
  # Sets up dataframe of simfrogs
  buffer <- 3
  if(fixed == F) {
    weight <- (buffer - 0)/((buffer - 0) + (pondsize - (pondsize-buffer)))
    which_region <- rbinom(n, 1, prob = weight)
    x <- runif(n, ifelse(which_region, 0, (pondsize-buffer)), ifelse(which_region, buffer, pondsize))
    y <- runif(n, ifelse(which_region, 0, (pondsize-buffer)), ifelse(which_region, buffer, pondsize))
  } else {
    weight <- (buffer - 0)/((buffer - 0) + (pondsize - (pondsize-buffer)))
    which_region <- rbinom(n, 1, prob = weight)
    set.seed(seed.x)
    x <- runif(n, ifelse(which_region, 0, (pondsize-buffer)), ifelse(which_region, buffer, pondsize))
    set.seed(seed.y)
    y <- runif(n, ifelse(which_region, 0, (pondsize-buffer)), ifelse(which_region, buffer, pondsize))
  }
  df <- as.data.frame(cbind(x,y))
  names(df) <- c("x", "y")
  df$id <- seq(1:n)
  df$sex <- sample(c("m", "f"), n, replace = T, prob = c(1-ratio, ratio))
  df
}


