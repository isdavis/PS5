
## Load libraries and set working directory
library(devtools)
library(roxygen2)
setwd("C:/Users/isdav/Documents/GitHub/PS5") #This will need to be changed to match your directory
getwd()
## This is run once when the package strcuture is first created

## At this point put the *.R files: integrateIt, printSimp and printTrap, plus the class R files
##into the correct directories and edit the DESCRIPTION file

## This can be run many times as the code is updated
current.code <- as.package("squaresPack")
load_all(current.code)
document(current.code)





