## =============================================================================
##
## Create a function that takes as its input a string representing an expression
## (ex. "is.data.table(my_variable"), and outputs the names of all funtions
## contained in the expression string.
##
## =============================================================================

identifyFunctions <- function(expression, output = NULL) {
    
    ## Define functions --------------------------------------------------------
    
    # Function for getting a numerical vector of the positions of a character
    # within a string (using regex, from the grepexpr function)
    getCharacterPositions <- function(string, pattern, ignore.case = FALSE) {
        charPositions <- gregexpr(pattern, string, ignore.case)
        charPositions <- charPositions[[1]][1:length(charPositions[[1]])]
        return(charPositions)
    }
    
    # Function that, given the position in a character vector of length one
    # of a left parenthese "(", gets the name of the function
    getFunction <- function(expression, parenPosition) {
        
        # Define symbols that are not allowed
        illegalSymbols <- c(" ", "<", "-", "+", "*", "(", '"', "'", ")",
                            "^", "&", "!", "@", "#", "$", "%", "/", "", 
                            "\\", "[", "]")
        
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
        # If currentFunction is "if" or "else", NULL it;
        # If a : is present in currentFunction, determine if its legal or not
        if (!is.na(suppressWarnings(as.numeric(currentFunction)))) {
            currentFunction <- NULL
            
        } else if (currentFunction %in% illegalSymbols) {
            currentFunction <- NULL
            
        } else if (currentFunction %in% c("if", "else")) {
            currentFunction <- NULL
            
        } else if (grepl(":", currentFunction)) {
            
            # If length(colonPositions) == 1, it's a sequence and we don't want
            # the :, it length is 2 or 3 it's good, if it's 4 or more it's back
            # to being bad!
            colonPositions <- getCharacterPositions(currentFunction, "\\:")
            if (length(colonPositions) == 1) {
                currentFunction <- substr(currentFunction, (colonPositions + 1),
                                          (nchar(currentFunction)))
            } else if (length(colonPositions) > 3) {
                firstColonPosition <- colonPositions[length(colonPositions)]
                currentFunction <- substr(currentFunction, (firstColonPosition + 1), 
                           (nchar(currentFunction)))
            }
        }
        
        # Return currentFunction when it's not NULL
        if (!is.null(currentFunction)) return(currentFunction)
    }
    
    ## Identify functions within the expression argument -----------------------
    
    # Get parenthesis locations
    parenLocations <- getCharacterPositions(expression, "\\(")
    
    # Loop over parenLocations and get the function names
    functionNames <- NULL
    if (length(parenLocations) != 0) {
        
        for (i in seq_along(parenLocations)) {
            
            currentParenLocation <- parenLocations[i]
            currentFunction <- getFunction(expression, currentParenLocation)
            if (!is.null(currentFunction)) {
                functionNames <- append(functionNames, currentFunction)
            }
        }
    }
    
    # Return functionNames
    return(functionNames)
}