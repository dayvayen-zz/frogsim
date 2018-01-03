#' Add parameters to male frogs
#'
#' @param tibs a tibble
#' @param temp temperature in degrees C (used for generating call rate)
#' @return a tibble similar to input but with frequency and call rate for male frogs
#' @export

add.parameters <- function(tibs, temp) {
  tibs$frequency <- rnorm(nrow(tibs), mean = 2390, sd = 142)
  tibs$callrate <- rnorm(nrow(tibs), mean = 30 + 1.6*temp, sd = 14)
  dplyr::mutate(tibs, frequency = ifelse(sex == "f", 0, frequency), callrate = ifelse(sex == "f", 0, callrate))
}
