#' changeColClass()
#'
#' Changes the class of columns from one class to another class, ex. change all
#' columns of class 'factor' into columns of class 'character.' Works on both
#' data.frames and data.tables. You may also specify either a) specific columns
#' to do this conversion on, or alternatively b) specific columns that you do not
#' want converted. 
#' 
#' Note that this function has side effects, namely, it modifies
#' an object in its calling environment. This was necessary for data.tables, as
#' the goal is to modify them in place. I decided to do the same for data
#' frames to make the function syntax the same, even though R won't do 
#' modification in place for data frames. 
#' 
#' @keywords convert, data.table, factor, numeric, character, data, table
#' @param data a data.frame or data.table
#' @param startingClass the column type you want converted
#' @param finalClass the column type you want to convert startingClass into
#' @param onlyConvert optional - specify a vector of one or more column names
#' as the only columns where the class conversion will take place. You may either
#' have this parameter satified, or the noConvert parameter specified, you may
#' not specify both.
#' @param noConvert optional - specify a vector of one or more columns names 
#' that you do not want any conversion applied to. You may either have this 
#' parameter satified, or the noConvert parameter specified, you may not 
#' specify both.
#' @export
#' @examples
#' 
#' changeColClass(my_data, "factor", "character")
#' changeColClass(my_data, "numeric", "character", onlyConvert = c(my_cols))
#' changeColClass(my_data, "numeric", "character", noConvert = "special_col")

changeColClass <- function(data = NULL, startingClass = NULL, finalClass = NULL,
                           onlyConvert = NULL, noConvert = NULL) {
    
    ## Error handling ----------------------------------------------------------
    
    # Missing arguments
    if (is.null(data)) stop("Please enter the data argument")
    if (is.null(startingClass)) stop("Requires argument for startingClass")
    if (is.null(finalClass)) stop("Requires argument for finalClass")
    
    # If data isn't a data.table or data.frame, get outta here
    if (!(is.data.frame(data) | is.data.table(data))) {
        stop("The data argument must be a data frame/table")
    }
    
    ## Get the column names of the columns that are of the class entered by
    ## the startingClass argument, and use NSE to get the name of data ---------
    
    # Get col_names
    sapply_exp <- paste0("sapply(data, is.", startingClass, ")")
    eval_this <- parse(text = sapply_exp)
    col_names <- names(data)[eval(eval_this)]
    
    # Deal with onlyConvert and noConvert
    if (!is.null(onlyConvert) & !is.null(noConvert)) {
        
        # If both onlyConvert and noConvert are specified, stop the function
        stop(paste0("Only one of the two optional arguments onlyConvert and ",
                    "noConvert may be specified at the same time."))
        
    } else if (!is.null(onlyConvert)) {
        
        # onlyConvert - take any col_names out if they are in onlyConvert 
        col_names <- col_names[col_names %in% onlyConvert]
        
    } else if (!is.null(noConvert)) {
        
        # noConvert - take any col_names out if they are in noConvert
        col_names <- col_names[!col_names %in% noConvert]
    }
    
    # If col_names is empty aka character(0), stop the function, there are
    # no columns of type startingClass in your data
    if (length(col_names) == 0) {
        print(sapply(data, class))
        stop(paste0("There were no columns found in the data of the ",
                    "type you entered in startingClass. Please check ",
                    "the above output of the class of each column ",
                    "in your data to see if the class you entered ",
                    "in startingClass exists."))
    }
    
    # Use NSE to get the name of the data argument
    data_name <- deparse(substitute(data)) 
    
    ## Do the conversion - different method based on data type
    
    # Define a function where you input an expression, and if the function(s)
    # in that expression exist, run the expression, if they don't output 
    # something (given as an argument to the function we are defining)
    checkExpression <- function(expression, outputIfBad = FALSE) {
        
        # Get the location of all of the parentheses
        parenLocations <- gregexpr("\\(", expression)
        
        # Function that, given the position in a character vector of length one
        # of a left parenthese "(", gets the name of the function
        getFunction <- function(expression, parenPosition) {
            
            # Define symbols that are not allowed
            illegalSymbols <- c(" ", "<", "-", "+", "*", "(", '"', "'",
                                "^", "&", "!", "@", "#", "$", "%", "/")
            
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
            
            if (currentChar %in% illegalSymbols) {
                return(invisible())
            } else {
                return(currentFunction)  
            }
        }
        
        # Loop over parenLocations and get the function names
        functionNames <- sapply(parenLocations[[1]], function(parenLocation) {
            return(getFunction(expression, parenLocation))
            currentFunction <- getFunction(expression, parenLocation)
            if (!is.null(currentFunction)) return(currentFunction)
        })
        
        print(functionNames)
        
        # Check if the function names exist
        functionExists <- sapply(functionNames, exists)
        
        print(functionExists)
        
        # If all functions exist, return the expression, and if not, return
        # outputIfBad
        if (all(functionExists == TRUE)) {
            return(eval(parse(text = expression)))
        } else {
            return(outputIfBad)
        }
    }
    
    ## //////////
    
    if (is.data.table(data)) {
        
        ## If data is a data.table ---------------------------------------------
        
        if (startingClass == "factor" & finalClass == "numeric") {
            
            # Special case where we are going from factor to numeric. In this 
            # case we can use the factorToNumeric function instead of 
            # as.numeric to make sure that we aren't converting the levels of
            data[, (col_names):=lapply(.SD, factorToNumeric), 
                 .SDcols = col_names]
            
        } else {
            
            # Convert col_names to the appropirate column type
            lapply_exp <- paste0("lapply(.SD, as.", finalClass, ")")
            eval_this <- parse(text = lapply_exp)
            data[, (col_names):=eval(eval_this), .SDcols = col_names]
        }
        
    } else {
        
        ## If data is a data.frame --------------------------------------------- 
        
        if (startingClass == "factor" & finalClass == "numeric") {
            
            # Special case where we are going from factor to numeric. In this 
            # case we can use the factorToNumeric function instead of 
            # as.numeric to make sure that we aren't converting the levels of
            data[col_names] <- lapply(data[col_names], factorToNumeric)
            
            # Modify in place (but copying the data frame) data
            command <- paste0(data_name, " <<- data")
            eval_this <- parse(text = command)
            eval(eval_this)
            
        } else {
            
            # Convert col_names to the appropirate column type
            lapply_exp <- paste0("data[col_names] <- lapply(data[col_names], as.", 
                                 finalClass, ")")
            eval_this <- parse(text = lapply_exp)
            eval(eval_this)
            
            # Modify in place (but copying the data frame) data
            command <- paste0(data_name, " <<- data")
            eval_this <- parse(text = command)
            eval(eval_this)
        }
    }
}