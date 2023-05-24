# function for median to mean
median2mean <- function(min = NA, q1 = NA, med = NA, q3 = NA, max = NA, n = NA) {
  if (is.na(q1) & is.na(q3) & !is.na(min) & !is.na(max)) {
    w = 4 / (4 + (n ^ 0.75))
    mean = w * ((min + max) / 2) + (1 - w) * med
    sd = (max - min) / (2 * qnorm(((n - 0.375) / (n + 0.25)), mean = 0, sd = 1))
  } else if (is.na(min) & is.na(max) & !is.na(q1) & !is.na(q3)){
    w = 0.7 + (0.39 / n)
    mean = w * ((q1 + q3) / 2) + (1 - w) * med
    sd = (q3 - q1) / (2 * qnorm((0.75 * n - 0.125) / (n + 0.25), mean = 0, sd = 1))
  } else if (!is.na(min) & !is.na(q1) & !is.na(med) & !is.na(q3) & !is.na(max) & !is.na(n)) {
    w1 = 2.2 / (2.2 + n ^ 0.75)
    w2 = 0.7 - (0.72 / (n ^ 0.55))
    mean = (w1 * (min + max)) / 2 + (w2 * (q1 + q3)) / 2 + (1 - w1 - w2) * med
    w3 = 1 / (1 + 0.07 * n ^ 0.6)
    sd = w3 * (max - min) / (2 * qnorm((n - 0.375) / (n + 0.25), mean = 0, sd = 1)) + ((1 - w3) * (q3 - q1)) / (2 * qnorm((0.75 * n - 0.125) / (n + 0.25), mean = 0, sd = 1))
  } else {
    warning("please inter correct values:1) q1, median, q3, and number 2) min, median, max, and number OR 3) all values")
  }
  list(mean, sd)
}
