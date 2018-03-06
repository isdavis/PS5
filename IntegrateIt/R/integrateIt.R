#' Integrating using simpsons rule and trapezoid rule
#'
#' Finds the solution of simpsons/trapezoid rule
#'
#' @param x A numeric vector
#' @param y A numeric vector with the same dimensionality as \code{x}.
#' @param ab A numeric vector equal to the max and min of \code{x}.
#' @param rule A character string, denoting either "Trap" or "Simpson"
#'
#' @return An object of class Trapezoid or Simpson, depending on \code{rule}, containing
#'  \item{x}{A numeric vector of x values}
#'  \item{y}{A numeric vector of corresponding y values, such that y=f(x) is a valid function.} 
#'  \item{ab}{The range, the max and min of the x values. }
#' @author Ian Davis
#' @note This is not a very simple function
#' @examples
#' 
#' myX <- c(1,2,3,4,5) 
#' myY <- c(1,2,3,2,1) 
#' myAB <-c(1,5)
#' myRule <-"Simpson"
#' integrateIt(myX, myY, myAB, myRule)
#' @seealso \code{\link{printSimp}}
#' @rdname integrateIt
#' @aliases integrateIt,ANY-method
#' @export
setGeneric(name="integrateIt",
           def=function(x, y, ab, rule, ...)
           {standardGeneric("integrateIt")}
)
#' @export
setMethod(f="integrateIt",
          definition=function(x, y, ab, rule, ...){
            if (rule=="Trap") {
              ret<-new("Trapezoid", x = x, y = y, ab = ab)
              result<-((ab[2]-ab[1])/length(x))/2*((2*sum(y))-y[1]-y[length(y)])
            }
            if (rule=="Simpson") {
              ret<-new("Simpson", x = x, y = y, ab = ab)
              z<-0
              for(i in 1:length(y)) {
                z<-z+2*y[i]+2*y[i]*((i-1) %% 2)
              }
              print(z)
              result<-((ab[2]-ab[1])/length(x))/3*(z-y[1]-y[length(y)])
              print(result)
            }
            return(list(ret, result))
          }
)

