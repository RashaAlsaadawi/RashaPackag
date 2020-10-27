#' Using the Pythagorean theorem
#'
#' pythag() takes two sides of a right triangle to get the third side
#'
#' @param side1 numeric, the first side of a right triangle
#' @param side2 numeric, the second side of a right triangle
#' @param hypotenuse numeric, the hypotenuse of a right triangle
#'
#' @return
#' @export
#'
#' @examples pythag(3,4)

pythag <- function(side1=0, side2=0, hypotenuse=0){

  if(is.numeric(side1) == FALSE | is.numeric(side2) == FALSE | is.numeric(hypotenuse) == FALSE){
    return('numeric values are needed')} else {

      if (side1>0 && side2>0 && hypotenuse>0) {
        stop("All three sides are already known")} else {

          if (side1>0 && side2>0 ) {
            hypotenuse=sqrt(side1^2 + side2^2)
            return(hypotenuse)

          }    else if (side1>0 && hypotenuse>0)  {
            side2=sqrt(hypotenuse^2 - side1^2)
            return(side2)

          }     else if (side2>0 && hypotenuse>0) {
            side1=sqrt(hypotenuse^2 - side2^2)
            return(side1)

          }      else {
            stop("Two positive lengths are needed")

          }
        }
    }
}

#Examples, uncomment to evaluate
#pythag(3,4)
#pythag(side1=6, hypotenuse=10)
#pythag(3,4,5)
#pythag(3,"a")
#pythag(3,-4)
