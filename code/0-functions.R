
# Returns a confidence interval from a vector of values
getCI <- function(data, ci = 0.95) {
  alpha <- (1 - ci)/2
  df <- data.frame(
    mean  = apply(data, 2, mean, na.rm = TRUE),
    lower = apply(data, 2, function(x) quantile(x, alpha, na.rm = TRUE)),
    upper = apply(data, 2, function(x) quantile(x, 1 - alpha, na.rm = TRUE)))
  return(df)
}

# Repeats a data frame n times
repDf <- function(df, n) {
  result <- df[rep(seq_len(nrow(df)), n), ]
  row.names(result) <- NULL
  return(result)
}