## ============================================================================
##
## Folder S3 object
## - Packages: assertthat
##
## ============================================================================

## Functions that are not for export ------------------------------------------

noEndingSlash <- function(dir) {
    
    # Function to ensure that a directory ends with a /
    if (substr(dir, nchar(dir), nchar(dir)) %in% c("\\", "/")) 
        dir <- substr(dir, 1, nchar(dir) - 1)
    dir
}

noStartingSlash <- function(dir) {
    
    # Function to erase a starting slash in a directory
    if (substr(dir, 1, 1) %in% c("\\", "/")) dir <- substr(dir, 2, nchar(dir))
    dir
}

combineRootSubDirs <- function(rootDir, subDir) {
    
    # Function to combine rootDir and subDir
    paste(noEndingSlash(rootDir), noStartingSlash(subDir), sep = "/")
}

## Class folder ---------------------------------------------------------------

folder <- function(root, ..., checkSubdirs = FALSE, defaultNames = TRUE, 
                   dirNames = NULL) {
    
    # Argument: rootDir - the root project directory. 
    # Argument: ... - one or more (possibly named) vectors indicating folders
    #           within the rootDir
    # Argument: checkSubdirs (default: FALSE) - check for the existance of the
    #           subdirs inside of rootDir
    # Argument: defaultNames (default: TRUE) - assigns names to elements of 
    #           ... that were not named, using base::basename(element) for the
    #           name of each unnamed element. Names that are not unique will
    #           result in the function returning an error
    # Argument: dirNames (optional) - a character vector of names to give to
    # Returns: an object of class 'folder'
    
    # Turn ... into a list, name the first one 'root'
    mylist <- list(root, ...)
    names(mylist)[1] <- "root"
    
    # Are all of the elements of mylist of length 1?
    if (any(vapply(mylist, function(f) length(f) != 1, logical(1)))) 
        stop("The length of each element of a folder object must be one.")
    
    # Assuming that more than just the root directory was inputted, run the
    # following:
    if (length(mylist) > 1) {
        
        # Do the subdirs exist (only if checkSubdirs is TRUE)?
        if (checkSubdirs & any(vapply(mylist[2:length(mylist)], function(f) {
            !dir.exists(combineRootSubDirs(mylist[[1]], f))
        }, logical(1)))) {
            stop("One or more of the subDirs does not exist.")
        }
        
        # Assign names to the elements of mylist depending on the defaultNames
        # and dirNames arguments
        if (!is.null(dirNames)) {
            names(mylist)[2:length(mylist)] <- dirNames
        } else if (defaultNames) {
            names(mylist) <- unlist(Map(function(name, dir) {
                if (is.na(name)) name <- basename(dir)
                name
            }, names(mylist), mylist, USE.NAMES = FALSE))
        }
        
        # If there are any repeat names, stop the function
        if (any(duplicated(names(mylist)))) {
            stop("One or more folder names (but not necessarily locations) are ",
                 "duplicates. Please correct this, or turn defaultNames to FALSE")
        }
    }
    
    # Return the object of class 'folder'
    structure(mylist, class = "folder")
}

# Function for determining if an object is a subsetObj
is.folder <- function(obj) inherits(obj, "folder")

# Print method for folder objects
print.folder <- function(folder) {
    
    # Turn folder into a list so that we can use list methods for subsetting
    class(folder) <- "list"
    
    # Print out info on the root directory
    cat("*Root dir* | subset with obj_name$root\n")
    cat(folder[[1]])
    cat("\n-----------------------------------------")
    
    # Print out info on the subDirs (if there are any)
    if (length(folder) > 1) {
        for (i in seq_along(folder[2:length(folder)])) {
            current_name <- names(folder)[i + 1]
            if (current_name == "") {
                subDirMsg <- paste0("Sub-directory ", i, " | subset with ",
                                    "obj_name", "[[", i + 1, "]]\n")
                
            } else if (grepl(" ", current_name)) {
                subDirMsg <- paste0("Sub-dir ", i, " | subset with ",
                                    "obj_name", '[["', current_name, '"]]\n')
                
            } else {
                subDirMsg <- paste0("Sub-dir ", i, " | subset with ",
                                    "obj_name", "$", current_name, "\n")
            }
            cat("\n")
            cat(subDirMsg)
            cat(combineRootSubDirs(folder[[1]], folder[[i + 1]]))
            cat("\n")
        }
    }
    
    cat("-----------------------------------------\n")
    cat("(where obj_name is the name of your folder object)\n")
}

createFolderPaths <- function(x) {
    
    # Takes a folder object, and replaces the sub-dirs with their relative
    # directory paths based on the root dir. This function is used internally
    # for the folder object's subsetting methods
    class(x) <- "list"
    if (length(x) > 1) {
        for (k in seq_along(x[2:length(x)])) {
            x[[k + 1]] <- combineRootSubDirs(x[[1]], x[[k + 1]])
        }
    }
    
    x
}

`[.folder` <- function(x, i) {
    
    # `[` method for the folder class
    x <- createFolderPaths(x)
    x[i]
}

`[[.folder` <- function(x, i, exact) {
    
    # `[[` method for the folder class
    x <- createFolderPaths(x)
    `[[`(x, i, exact = exact)
}

`$.folder` <- function(x, i) {
    
    # `$` method for the folder class
    x <- createFolderPaths(x)
    structure(`[[`(x, i, exact = FALSE), class = c("folderstring", 
                                                   "character"))
}

`+.folderstring` <- function(e1, e2) {
    paste0(e1, e2)
}

# root function to return the root directory of a folder object
root <- function(folder) {
    UseMethod("root")
}

root.folder <- function(folder) {
    folder$root
}

# subDir function to subset a folder object to get a specific subDir back.
# Different than normal subsetting in that if a numeric index is inputted,
# 1 is added to each element in the index so that the numeric index corresponds
# to the subDirs, NOT to the position in the entire folder (including the root)
subDir <- function(folder, subDir) {
    UseMethod("subDir")
}

subDir.folder <- function(folder, subDir) {
    
    # Error handling
    assertthat::assert_that(length(subDir) == 1)
    assertthat::assert_that(length(folder) > 1)
    
    # Return the subDir
    if (is.numeric(subDir)) subDir <- subDir + 1
    folder[[subDir]]
}