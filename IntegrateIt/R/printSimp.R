#'  Printing result of the Simpson Rule 
#'
#' Prints the result of the Simpson rule, given an object of class Simpson 
#'
#' @param object An object of class Simpson 
#'
#' @return An evaluation of the Simpson rule for the given object of class Simpson
#' @author Ian Davis
#' @note This is a very simple function
#' @examples
#' 
#' i<-new("Simpson", x=c(1,2,3,4,5), y=c(1,2,3,1,1), ab=c(1,5))
#' printSimp(i)
#' 
#' @seealso \code{\link{printSimp}}
#' @rdname printSimp
#' @aliases printSimp,ANY-method
#' @export
setGeneric(name="printSimp",
           def=function(object="Simpson")
           {standardGeneric("printSimp")}
)
#' @export
setMethod(f="printSimp",
          definition=function(object="Simpson"){
            z<-0
            for(i in 1:length(object@y)) {
              z<-z+2*object@y[i]+2*object@y[i]*((i-1) %% 2)
            }
            result<-((object@ab[2]-object@ab[1])/length(object@x))/3*(z-object@y[1]-object@y[length(object@y)])
            print(result)
            return(result)})


