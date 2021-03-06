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
#' that doesn't use NSE.
#'
#' @param oldObj the object that should have its name changed
#' @param newObj the new name of the object
#' @param overwriteExisting TRUE by default, should the function overwrite
#' existing objects with the name newObj?
#' @export
#' @examples
#' 
#' jake <- c(5, 6, 7)
#' changeObjectName(jake, john)

changeObjectName <- function(oldObj, newObj, overwriteExisting = TRUE) {
    
    # Use NSE to get names of oldObj, newObj
    oldObjName <- deparse(substitute(oldObj))
    newObjName <- deparse(substitute(newObj))
    
    # Error handling
    assert_that(exists(oldObjName))
    assert_that(notIdentical(oldObjName, newObjName))
    
    # If an object named newObjName exists, throw a warning, but don't 
    # overwrite it if overwriteExisting == FALSE
    if (exists(newObjName)) {
        if (overwriteExisting) {
            warning("An object named: '", newObjName, 
                    "' already exists and is being overwritten.")
        } else {
            stop("An object named: '", newObjName , 
                 "' already exists, stopping the function.")
        }
    }
    
    # Create an object w/ a name of newObj associated with the value of oldObj
    assign(newObjName, oldObj, envir = sys.frame(-1))
    
    # Remove oldObj
    rm(list = oldObjName, envir = sys.frame(-1))
}

#' changeObjectName_()
#' 
#' Change the name of an object. Uses standard evaluation (SE) to capture
#' to change the name of an object given character vectors for the current
#' name of an object and the desired name.
#'
#' @param oldObj the object that should have its name changed
#' @param newObj the new name of the object
#' @param overwriteExisting TRUE by default, should the function overwrite
#' existing objects with the name newObj?
#' @export
#' @examples
#' 
#' jake <- c(5, 6, 7)
#' changeObjectName("jake", "john")

changeObjectName_ <- function(oldObj, newObj, overwriteExisting = TRUE) {
    
    # Error handling 
    assert_that(is.string(oldObj))
    assert_that(is.string(newObj))
    assert_that(is.flag(verbose))
    
    # If an object named newObj exists, throw a warning, but don't 
    # overwrite it if overwriteExisting == FALSE
    if (exists(newObj)) {
        if (overwriteExisting) {
            warning(newObj, " already exists and is being overwritten.")
        } else {
            stop(newObj ," already exists, stopping the function.")
        }
    }
    
    # Create an object w/ a name of newObj associated with the value of oldObj
    assign(newObj, get(oldObj, envir = sys.frame(-1)), envir = sys.frame(-1))
    
    # Remove oldObj
    rm(list = oldObj, envir = sys.frame(-1))
}

#' listToObjects()
#'
#' Turns each element of a list into an object in your global environment with
#' the name of each element in the list becoming the name of that object. By
#' default, the list is deleted at the end, although this behavior may be 
#' turned off. 
#' 
#' @keywords transform, list, into, objects
#' @param myList a list
#' @param deleteList TRUE by default, should the list be deleted after?
#' @param objectNames (optional) a vector of object names, one per element in
#' myList. By default, the names of the objects will be the names of myList,
#' which may be viewed via \code{names(myList)}
#' @param overwriteExisting TRUE by default, should the function overwrite
#' existing objects whose names are in \code{names(myList)} or the objectNames
#' argument?
#' @param verbose TRUE by default, should the function be verbose?
#' @export
#' @examples 
#' 
#' jake <- list(a = c(1, 2, 3), b = c(2, 3, 4))
#' listToObjects(jake)

listToObjects <- function(myList, deleteList = TRUE, objectNames = NULL,
                          overwriteExisting = TRUE, verbose = TRUE) {
    
    # Use NSE to get the name of the list
    myListName <- deparse(substitute(myList))
    
    # Error handling
    assert_that(exists(myListName))
    assert_that(is.flag(deleteList))
    assert_that(is.flag(verbose))
    if (!is.null(objectNames) & length(objectNames) != length(myList)) {
        stop("The number of objectNames does not match the length of your ",
             "list.")
    }
    
    objectNamesExist <- !is.null(objectNames)
    
    ##  Assign elements of the list to objects
    
    for (i in seq_along(myList)) {
        
        # Get the name for the object
        if (objectNamesExist) {
            elementName <- objectNames[i]  
        } else {
            elementName <- names(myList)[i]  
        }
        
        # What to do if the object already exists?
        if (exists(elementName)) {
            if (overwriteExisting) {
                if (verbose) message("The object named: '", elementName , "' ",
                                     "already exists, stopping the function.")
            } else {
                stop("The object named: '", elementName ,"' already exists, ",
                     "stopping the function.")
            }
        }
        
        # Create the object
        assign(elementName, myList[[i]], envir = sys.frame(-1))
        if (verbose) message("Element: ", elementName, 
                             " assigned to an object.")
    }
    
    ## Delete the list if deleteList == TRUE
    
    if (deleteList) {
        rm(list = myListName, envir = sys.frame(-1))
        if (verbose) message("\nList: ", myListName, 
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
#' 
#' createDirIfNotExist("C:/Users/Jake/Programming/NewFolder")

createDirIfNotExist <- function(dir_location, ...) {
    
    if (!file.exists(dir_location)) {
        dir.create(dir_location, ...)
    } else {
        message(dir_location, " already exists.")
    }
}

#' splitRmNA()
#'
#' Splits one vector by another vector after removing elements from both
#' vectors when either vector is NA. 
#' 
#' @keywords split, vector, remove, NA, NAs
#' @param vector1
#' @param vector2
#' @param ... additional arguments to be passed to \code{base::split}
#' @export
#' 
#' splitRmNA(vector1, vector2)

splitRmNA <- function(vector1, vector2, ...) {
    
    # Error handling 
    assert_that(isVector(vector1))
    assert_that(isVector(vector2))
    
    # Split the vectors
    subsetBy <- complete.cases(vector1, vector2)
    vector1 <- vector1[subsetBy]
    vector2 <- vector2[subsetBy]
    split(vector1, vector2, ...)
}

# Ensures the column classes of a data.table, this is a work-in-progress, 
# this version will become the data.table method, but I also need to create
# a data.frame method
ensureClass <- function(data, classes) {
    
    # Error handling
    stopifnot(length(data) == length(classes))
    stopifnot(inherits(data, "data.table"))
    
    # Loop over the columns of data
    for (i in seq_along(data)) {
        if (class(data[[i]]) != classes[i]) { 
            old_class <- class(data[[i]])
            conversion_func <- match.fun(paste0("as.", classes[i]))
            set(data, j = names(data)[i], value = conversion_func(data[[i]]))
            message("Column ", names(data)[i], " converted from ", old_class, 
                    " to ", classes[i], " by reference.")
        }
    }
}