is_count <- function(x) {
  if (length(x) != 1 || !is.numeric(x) || x <= 0) {
    return(FALSE)
  }
  is.integer(x) || (is.numeric(x) && all(x == as.integer(x)))
}
