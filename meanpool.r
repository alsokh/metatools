# function to merge mean of the measures of two or more studies
# this function is mainly used for merging multiple measures of a study
# this is a method to overcome unit-of-anlysis error

# it gets the number of measures, mean of each effect, and standard deviation and merge the studies keeping the number of measures
mean_pool <- function(n, means, sds) {
  x <- 0
  y <- 0
  for (i in (1:length(means))) {
    x <- x + n[i] * means[i] 
  }
  pooled_mean <- x / sum(n)
  for (i in (1:length(sds))) {
    y <- y + (((sds[i] ** 2) * ((n[i] - 1) / n[i])) + (means[i] ** 2)) * n[i]
  }
  pooled_sd <- sqrt(((y / sum(n)) - (pooled_mean ** 2)) * (sum(n) / (sum(n) - 1)))
  c(round(pooled_mean, digits = 2), round(pooled_sd, digits = 2))
}

