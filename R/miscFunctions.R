## ============================================================================
##
## Misc. functions
##
## ============================================================================

#' changeObjectName()
#' 
#' Change the name of an object. Uses non-standard evaluation (NSE) to capture
#' the unevaluted expressions given for its arguments (this should become clear
#' in the examples below.) See changeObjectName_ for a version of this function
#' that doesn't use NSE
#'
#' @param oldObj the object that should have its name changed
#' @param newObj the new name of the object
#' @export
#' @examples
#' 
#' jake <- c(5, 6, 7)
#' changeObectName(jake, john)

changeObjectName <- function(oldObj, newObj) {
    
    # Arguments: an object (oldObj), the name that you want to change oldObj 
    #            to (newObj)
    # Side effects: Creates newObj, assigns it the value of oldObj, and then
    #               removes oldObj
    
    # Use NSE to get names of oldObj, newObj
    oldObjName <- deparse(substitute(oldObj))
    newObjName <- deparse(substitute(newObj))
    
    # Error handling
    assert_that(exists(oldObjName))
    
    # Create an object w/ a name of newObj associated with the value of oldObj
    assign(newObjName, oldObj, envir = sys.frame(-1))
    
    # Remove oldObj
    rm(list = oldObjName, envir = sys.frame(-1))
}

changeObjectName_ <- function(oldObj, newObj) {
    
    # Arguments: an object (oldObj), the name that you want to change oldObj 
    #            to (newObj), all strings (no NSE)
    # Side effects: Creates newObj, assigns it the value of oldObj, and then
    #               removes oldObj
    
    # Error handling
    
    
    # Create an object w/ a name of newObj associated with the value of oldObj
    assign(newObj, get(oldObj, envir = sys.frame(-1)), envir = sys.frame(-1))
    
    # Remove oldObj
    rm(list = oldObj, envir = sys.frame(-1))
}

listToObjects <- function(myList, deleteList = TRUE, verbose = TRUE) {
    
    # Arguments: a list, where each element is named
    # Side effects: saves each element of the list as an object in the next up
    #               environment with the name of the element of the list. Once
    #               the objects are created, the list will be removed
    
    # Use NSE to get the name of the list
    myListName <- deparse(substitute(myList))
    
    # Assign elements of the list to objects
    for (i in seq_along(myList)) {
        elementName <- names(myList)[i]
        assign(elementName, myList[[i]], envir = sys.frame(-1))
        if (verbose) message("Element: ", elementName, 
                             " assigned to an object.")
    }
    
    # Delete the list if deleteList == TRUE
    if (deleteList) {
        rm(list = myListName, envir = sys.frame(-1))
        if (verbose) message("List: ", myListName, 
                             " removed, all objects assigned.")
    }
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
