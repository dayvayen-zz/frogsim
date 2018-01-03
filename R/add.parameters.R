# what if instead i just made a new thing with the tibs and the df that adds in freq and callrate so you don't have to use it if you don't wanna?


add.parameters <- function(tibs, temp) {
  tibs$frequency <- rnorm(nrow(tibs), mean = 2390, sd = 142)
  tibs$callrate <- rnorm(nrow(tibs), mean = 30 + 1.6*temp, sd = 14)
  dplyr::mutate(tibs, frequency = ifelse(sex == "f", 0, frequency), callrate = ifelse(sex == "f", 0, callrate))
}
