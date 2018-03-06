setwd("C:/Users/isdav/Documents/GitHub/PS5")

#This is PS5! This first bit is adapted from PS4 to create the validity checker and classes. 

#This checks the values for x, y, and ab, which is the range. 
validT<-function(object) {
  x<-object@x
  y<-object@y
  ab<-object@ab
  errors<-character()
  if(length(ab)!=2) {errors<-"Invalid range."}
  if(length(x)<2) {errors<-"At least 2 points required to construct a curve."}
  if(ab[1]!=x[1] | ab[length(ab)]!=x[length(x)]) {errors<-"X values must match range."}
  if(length(x)!=length(y)) {errors<-"This is not a function."} 
  if(length(errors)==0) return(TRUE) else return(errors)
}

setClass(Class="Trapezoid",
         representation = representation(
           x = "numeric",
           y = "numeric",
           ab = "numeric"
         ),
         prototype = prototype(
           x = c(),
           y = c(),
           ab = c()
         ), validity = validT
)



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

#This is an example to test the validity checkers; it can be changed around however you like. 
b<-new("Simpson", x=c(1,2,3,4), y=c(1,2,3,9), ab=c(1,4))

setMethod("initialize", "valid", function(.Object, ...) {
  value = callNextMethod()
  validObject(value)
  return(value)
})

setGeneric(name="integrateIt",
           def=function(x, y, ab, rule, ...)
           {standardGeneric("integrateIt")}
)

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
              result<-((ab[2]-ab[1])/length(x))/3*(z-y[1]-y[length(y)])
              print(result)
            }
            return(list(ret, result))
          }
)

integrateIt(c(1,2,3,4,5), c(1,2,3,4,5), c(1,5), "Trap")
integrateIt(c(1,2,3,4,5), c(1,2,3,4,5), c(1,5), "Simpson")

setGeneric(name="addSquares",
           def=function(x, y, ...)
           {standardGeneric("addSquares")}
)

setMethod(f="addSquares",
          definition=function(x, y, ...){
            return(new("Squares", square=(x^2 + y^2), x = x, y = y))
          }
)

addSquares(3,3)

