
## Load libraries and set working directory
library(devtools)
library(roxygen2)
setwd("C:/Users/Ian Davis/Documents/GitHub/PS5") #This will need to be changed to match your directory
getwd()

##package.skeleton(name="IntegrateIt", code_files = ) 

##I am still trying to figure out how package.skeleton works for S4; it would have made this whole assignment much easier.

## This is run once when the package strcuture is first created

## At this point put the *.R files: integrateIt, printSimp and printTrap, plus the class R files (Trapezoid.R and Simpson.R)
##into the correct directories and edit the DESCRIPTION file

## This can be run many times as the code is updated
current.code <- as.package("IntegrateIt")
load_all(current.code)
document(current.code)





