#'  Printing result of the Trapezoid Rule 
#'
#' Prints the result of the Trapezoid rule, given an object of class Trapezoid
#'
#' @param object An object of class Simpson 
#'
#' @return An evaluation of the Trapezoid rule for the given object of class Trapezoid
#' @author Ian Davis
#' @note This is a very simple function
#' @examples
#' 
#' b<-new("Trapezoid", x=c(1,2,3,4,5), y=c(1,2,3,1,1), ab=c(1,5))
#' printTrap(b)
#' 
#' @seealso \code{\link{integrateIt}}
#' @rdname printTrap
#' @aliases printTrap,ANY-method
#' @export
setGeneric(name="printTrap",
           function(object)
           {standardGeneric("printTrap")}
)
#' @export
setMethod("printTrap", signature(object="Trapezoid"),
          function(object){
            result<-((object@ab[2]-object@ab[1])/(length(object@x)-1))/2*((2*sum(object@y))-object@y[1]-object@y[length(object@y)])
            print(result)
            return(result)})