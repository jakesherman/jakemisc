## =============================================================================
##
## Given a function name, checks whether or not the function exists, and which
## environment the function comes from. Output is a list, each element contains
## 1) TRUE or FALSE if the function exists, 2) if TRUE, the name of the 
## function's enclosing environment 
##
## See: http://adv-r.had.co.nz/Environments.html
##
## =============================================================================

whereFunction <- function(functionName, output = "environment", 
                          multipleFunctions = FALSE, env = parent.frame(),
                          warnings = TRUE) {
    
    ## Error handling ----------------------------------------------------------
    
    if (!(output %in% c("environment", "logical"))) {
        stop("output must be set to either environment or logical")
    }
    
    ## Define functions and parameters -----------------------------------------
    
    # Define a logical vector for when output is "logical", and a list when
    # output is "list"
    logicalOutput <- vector()
    listOutput <- list()
    
    # Create a function that gets function names from :: or ::: operators
    colonFunctionSeperation <- function(functionName) {
        
        # Get the colon positions in functionName, if there are not 2 or 3
        # colons, return NULL
        colonPositions <- patternPositions(functionName, "\\:")
        if (length(colonPositions) < 2 | length(colonPositions) > 3) {
            return(NULL)
        } else {
            packageName <- substr(functionName, 1, colonPositions[1])
            functionName <- substr(functionName, 
                                   colonPositions[length(functionName)], 
                                   nchar(functionName))
            return(list(packageName = packageName, functionName = functionName))
        }
    }
    
    # Create a recursive function that finds if a function exists, and if it
    # exists which environment it is in. Has the ability to look through all
    # environments to identify functions with the same name specified in 
    # multiple environments on the search list. Will only find functions that
    # have been exported (aka are in the package environment, not the namespace
    # environment). When onlyCheckEnv is TRUE, it only searches the environment
    # that it is given by the env argument.
    
    whereFunctionInternal <- function(functionName, env, onlyCheckEnv = FALSE) {
        
        if (identical(env, emptyenv())) {
            
            # Base - the function doesn't exist
            logicalOutput <<- FALSE
            listOutput <<- logicalOutput 
            if (interactive() & warnings) warning("Can't find ", functionName)
            
        } else if (exists(functionName, envir = env, inherits = FALSE, 
                          mode = "function")) {
            
            # Sucess - the function exists
            logicalOutput <<- c(logicalOutput, TRUE)
            listOutput <<- c(listOutput, env)
            
            # Recursive - keep looking for the function (if multipleFunctions
            # is set to TRUE from its default of FALSE)
            if (multipleFunctions) {
                whereFunctionInternal(functionName, parent.env(env))
            }
            
        } else {
            
            # If onlyCheckEnv is TRUE, don't keep looking for the function
            if (onlyCheckEnv) {
                logicalOutput <<- FALSE
                listOutput <<- logicalOutput 
                return(NULL)
            }
            
            # Recursive - keep looking for the function
            whereFunctionInternal(functionName, parent.env(env))
        } 
    }
    
    ## Fun stuff ---------------------------------------------------------------
    
    # If a : is in functionName, check to see if it's a :: or ::: operator, and
    # if so check to see if the function exists in the namespace specified 
    # before the colon operator, and just return that function. If : is not
    # present, recurse through all environments in the search list with the
    # whereFunctionInternal function
    
    if (grepl(":", functionName)) {
        
        # Seperate the package from the function from the :: or :::
        functionInfo <- colonFunctionSeperation(functionName)
        
        # Check to see if the package namespace exists, if it does, assign it
        # to package_env, if not return NULL and throw a warning for 
        # interactive users
        if(requireNamespace(functionInfo[["packageName"]])) {
            # Success
            package_env = loadNamespace(functionInfo[["packageName"]])
            
        } else {
            # Failure
            if (interactive()) warning("function not found in given package")
            return(NULL)
        }
        
        # If the function exists in the given package namespace, sucess, if
        # not failure (and return NULL)
        if (exists(functionInfo[["functionName"]], envir = package_env,
                   mode = "function")) {
            # Success
            logicalOutput <<- c(logicalOutput, TRUE)
            listOutput <<- c(listOutput, env)
            
        } else {
            # Failure
            return(NULL)
        } 
        
    } else {
        
        # Run whereFunctionInternal
        whereFunctionInternal(functionName, env)
    }
    
    # Return either the logical or the list depending on our desired output
    if (output == "logical") return(logicalOutput)
    return(listOutput)
}