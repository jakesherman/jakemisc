# Get identifyFunctions
setwd("C:/Users/Jake/Programming/jakemisc/R")
source("identifyFunctions.R")

# Read in the lines of the R code
rscript <- readLines("identifyFunctions.R", encoding = "UTF-8")

# Use identifyFunctions on each line
allFunctions <- sapply(rscript, identifyFunctions, USE.NAMES = FALSE)
