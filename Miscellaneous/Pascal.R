# R function to give an element of Pascal's triangle

# The idea for this function is directly lifted from one of the 'Structure
# and Interpretation of Computer Programs' exercises. The book works with
# a dialect of LISP which was one of the influences on the R language.

# The function uses recursion to return one of the elements of Pascal's
# triangle. In the spirit of R I've used 1-based indexing instead of
# 0-based indexing. So the first row of the triangle for this function is
# '1' instead of the traditional '0', and the first element in a row is '1'
# instead of '0'.

# Inputs:
# - rownum: the row of the triangle
# - element: the element within the row to be returned

pascal <- function(rownum, element=NULL) {

  # Input Validation
  if (!is.numeric(rownum)) {
    stop("Error: rownum must be numeric")
  } else if (rownum %% 1 != 0) {
    stop("Error: value of rownum must be integer")
  } else if (length(rownum) > 1) {
    stop("Error: rownum should be a single number")
  }

  if (!is.null(element)) {
    if (!is.numeric(element)) {
      stop("Error: element must be NULL or numeric")
    } else if (element %% 1 != 0) {
      stop("Error: value of element must be integer")
    } else if (length(element) > 1) {
      stop("Error: element should be a single number")
    } else if (element > rownum) {
      stop("Error: element must be less than or equal to rownum")
    }
  }

  if (rownum > 1 & is.null(element)) {
    stop("Error: For rownum greater than 1, element must be specified")
  }

  # Code
  if (rownum == 1) {
    return(1)
  } else if (element %in% c(1, rownum)) {
    return(1)
  } else {
    return(pascal(rownum - 1, element - 1) + pascal(rownum - 1, element))
  }

}






