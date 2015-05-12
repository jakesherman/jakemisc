## ============================================================================
##
## Misc. functions
##
## ============================================================================

changeObjName <- function(oldObj, newObj) {
    
    # Arguments: an object (oldObj), the name that you want to change oldObj 
    #            to (newObj)
    # Side effects: Creates newObj, assigns it the value of oldObj, and then
    #               removes oldObj
    
    # Use NSE to get names of oldObj, newObj
    oldObjName <- deparse(substitute(oldObj))
    newObjName <- deparse(substitute(newObj))
    
    # Create an object w/ a name of newObj associated with the value of oldObj
    changeNameExpression <- paste0(newObjName, " <<- ", oldObjName)
    eval(parse(text = changeNameExpression))
    
    # Remove oldObj
    rm(list = oldObjName, envir = sys.frame(-1))
}

#' createDirIfNotExist()
#'
#' Creates a directory if it doesn't already exist.
#' 
#' @keywords create, dir, directory, if, not, exist, existing
#' @param dir_location the directory you want to create
#' @param ... additional arguments to be passed to dir.create
#' @export

createDirIfNotExist <- function(dir_location, ...) {
    
    if (!file.exists(dir_location)) {
        dir.create(dir_location, ...)
    } else {
        message(dir_location, " already exists.")
    }
}
