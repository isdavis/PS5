#' A Simpson rule object 
#' 
#' Object of class \code{Simpson} are created by the \code{integrateIt} functions
#'
#' 
#' An object of the class `Simpson' has the following slots:
#' \itemize{
#' \item \code{x} A numeric vector of x values
#' \item \code{y} A numeric vector of corresponding y values, such that y=f(x) is a valid function.
#' \item \code{ab} The range, the max and min of the x values
#' @author Ian Davis
#' @aliases Simpson-class initialize,Simpson validity checker 
#' @rdname Simpson
#' @export
validS<-function(object) {
x<-object@x
y<-object@y
ab<-object@ab
errors<-character()
if(length(ab)!=2) {errors<-"Invalid range."}
if((length(x) %% 2)==0) {errors<-"For Simpson rule, n must be odd."}
if(length(x)<2) {errors<-"At least 2 points required to construct a curve."}
else if((x[2]-x[1])!=x[length(x)]-x[length(x)-1]) {errors<-"X values must be evenly spaced."}
#Elementary test to see if the points are evenly spaced
if(ab[1]!=x[1] | ab[length(ab)]!=x[length(x)]) {errors<-"X values must match range."}
if(length(x)!=length(y)) {errors<-"This is not a function."} 
if(length(errors)==0) return(TRUE) else return(errors)}

#' @export
setClass(Class="Simpson",
         representation = representation(
           x = "numeric",
           y = "numeric",
           ab = "numeric"
         ), 
         prototype = prototype(
           x = c(),
           y = c(),
           ab = c()
         ), validity = validS
)

#' @export
setMethod("initialize", "Simpson", 
function(.Object, ...){
  value=callNextMethod()
  return(value)
}
) 

