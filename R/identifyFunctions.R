## =============================================================================
##
## Create a function that takes as its input a string representing an expression
## (ex. "is.data.table(my_variable"), and outputs the names of all funtions
## contained in the expression string.
##
## =============================================================================

identifyFunctions <- function(expression, output = NULL) {
    
    # Get the location of all of the parentheses
    parenLocations <- gregexpr("\\(", expression)
    parenLocations <- parenLocations[[1]][1:length(parenLocations[[1]])]
    
    # Function that, given the position in a character vector of length one
    # of a left parenthese "(", gets the name of the function
    getFunction <- function(expression, parenPosition) {
        
        # Define symbols that are not allowed
        illegalSymbols <- c(" ", "<", "-", "+", "*", "(", '"', "'", ")",
                            "^", "&", "!", "@", "#", "$", "%", "/", "")
        
        ## Loop over positions starting just past the left paren position,
        ## while they aren't in illegalSymbols, keep going
        
        currentPosition <- parenPosition - 1
        currentChar <- substr(expression, currentPosition, currentPosition)
        
        while (!(currentChar %in% illegalSymbols)) {
            
            currentPosition <- currentPosition - 1
            currentChar <- substr(expression, currentPosition, 
                                  currentPosition)
            if (currentPosition == 0) break
        }   
        
        # Get the current function
        currentFunction <- substr(expression, (currentPosition + 1), 
                                  (parenPosition - 1))
        
        # If currentFunction is a number, ex. in the case of 8(5+3), NULL it;
        # If currentFunction is one of the illegal symbols, NULL it;
        # If currentFunction is "if" or "else", NULL it
        if (!is.na(suppressWarnings(as.numeric(currentFunction)))) {
            currentFunction <- NULL
        } else if (currentFunction %in% illegalSymbols) {
            currentFunction <- NULL
        } else if (currentFunction %in% c("if", "else")) {
            currentFunction <- NULL
        }
        
        # Return currentFunction when it's not NULL
        if (!is.null(currentFunction)) return(currentFunction)
    }
    
    # Loop over parenLocations and get the function names
    functionNames <- NULL
    if (length(parenLocations) == 0) {
    } else {
        for (i in seq_along(parenLocations)) {
            
            currentParenLocation <- parenLocations[i]
            currentFunction <- getFunction(expression, currentParenLocation)
            if (!is.null(currentFunction)) {
                functionNames <- append(functionNames, currentFunction)
            }
        }
    }
    
    # If functionNames is NULL, return nothing
    if (is.null(functionNames)) {
        return(invisible())
    } else {
        return(functionNames)
    }
}