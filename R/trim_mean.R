#' Get trimmed mean of a numeric vector
#'
#' This function gets the mean of a numeric vector after trimming
#' the smallest s numbers and the largest l numbers from the vector
#'
#' @param x numeric vector, with length > s+l
#' @param s integer, number of smallest numbers to be trimmed
#' @param l integer, number of largest numbers to be trimmed
#'
#' @return
#' @export
#' @examples trim_mean(c(3,2.5,6,1,12,5.5,20),1,2)

trim_mean <- function(x,s,l){

  if (length(x)> s+l) {
    trim_x <- sort(x)[-c(1:s)]
    trim_x <- trim_x[1:(length(trim_x) -l)]
    mean <- mean(trim_x)
    return(mean)

  } else { stop("Not enough elements in vector x")
  }
}
