#' R function to return a single entry from Pascal's triangle
#'
#' The idea for this function is lifted from one of the 'Structure
#' and Interpretation of Computer Programs' exercises. It uses
#' recursion to return one of the entries of Pascal's triangle.
#'
#' To stay in line with conventions for the triangle, unlike most of R
#' the function uses 0-based indexing for the triangle.
#' This means the first row of the triangle is 0, and the first entry
#' of a row is 0.
#'
#' Rows are indexed by n, while entries are indexed by k.

pascal <- function(n, k=NULL) {

  # Input Validation
  if (!is.numeric(n)) {
    stop("Error: n must be numeric")
  } else if (n %% 1 != 0 | n < 0) {
    stop("Error: value of n must be a natural number")
  } else if (length(n) > 1) {
    stop("Error: n should be a single number")
  }

  if (!is.null(k)) {
    if (!is.numeric(k)) {
      stop("k must be NULL or numeric")
    } else if (k %% 1 != 0 | k < 0) {
      stop("value of k must be a natural number")
    } else if (length(k) > 1) {
      stop("k should be a single number")
    } else if (k > n) {
      stop("k must be less than or equal to n")
    }
  }

  if (n > 0 & is.null(k)) {
    stop("for n greater than 0, k must be specified")
  }

  # Code
  if (n == 0) {
    return(1)
  } else if (k %in% c(0, n)) {
    return(1)
  } else {
    return(pascal(n - 1, k - 1) + pascal(n - 1, k))
  }

}






