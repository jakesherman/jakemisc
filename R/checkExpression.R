# Define a function where you input an expression, and if the function(s)
# in that expression exist, run the expression, if they don't output 
# something (given as an argument to the function we are defining)

checkExpression <- function(expression, outputIfBad = FALSE) {
    
    # Get the location of all of the parentheses
    parenLocations <- gregexpr("\\(", expression)
    parenLocations <- parenLocations[[1]][1:length(parenLocations[[1]])]
    
    # Function that, given the position in a character vector of length one
    # of a left parenthese "(", gets the name of the function
    getFunction <- function(expression, parenPosition) {
        
        # Define symbols that are not allowed
        illegalSymbols <- c(" ", "<", "-", "+", "*", "(", '"', "'",
                            "^", "&", "!", "@", "#", "$", "%", "/", "")
        
        ## Loop over positions starting just past the left paren position,
        ## while they aren't in illegalSymbols, keep going
        
        currentPosition <- parenPosition - 1
        currentChar <- substr(expression, currentPosition, currentPosition)
        
        while (!(currentChar %in% illegalSymbols)) {
            
            currentPosition <- currentPosition - 1
            currentChar <- substr(expression, currentPosition, 
                                  currentPosition)
            if (currentPosition == 1) break
        }   
        
        # Get the current function
        currentFunction <- substr(expression, (currentPosition + 1), 
                                  (parenPosition - 1))
        
        if (currentFunction %in% illegalSymbols) {
            
        } else {
            return(currentFunction)  
        }
    }
    
    # Loop over parenLocations and get the function names
    functionNames <- sapply(parenLocations, function(parenLocation) {
        return(getFunction(expression, parenLocation))
    })
    
    print(functionNames)
    
    # Check if the function names exist
    functionExists <- sapply(functionNames, exists)
    
    # If all functions exist, return the expression, and if not, return
    # outputIfBad
    if (all(functionExists == TRUE)) {
        return(eval(parse(text = expression)))
    } else {
        return(outputIfBad)
    }
}

## checkExpression example

data <- data.frame(a = c(1,2,3))
checkExpression("is.data.table(as.data.frame(data))")  # should return false

# Testing getting objects
test <- objects()
test <- objects(package:graphics)
jake <- test[sapply(test, function(f) {
    ifelse(class(f) == "function", TRUE, FALSE)
})]

class(as.data.frame)
class(graphics::Axis)