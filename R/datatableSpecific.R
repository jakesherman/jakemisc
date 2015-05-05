## ============================================================================
##
## Functions specific to the data.table package
##
## ============================================================================

# Assertions:

notNULL <- function(x) !is.null(x)

assertthat::on_failure(notNULL) <- function(call, env) {
    paste0("Please fill in the '", deparse(call$x), "' argument.")
}

is.DT <- function(x) inherits(x, "data.table")

assertthat::on_failure(is.DT) <- function(call, env) {
    paste0(deparse(call$x), " is not a data.table.")
}

namesIn <- function(names, df) {
    
    # Arguments: character vector (names), a data.frame (df) w/ names
    # Outputs: are all of names in names(df)? TRUE or FALSE.
    
    all(names %in% names(df))
}

assertthat::on_failure(namesIn) <- function(call, env = parent.env) {
    paste0("One or more of the names in '", deparse(call$x), "' is not a name",
           " in the data.frame")
}

columnsToUse <- function(allCols, markedCols) {
    
    # Error handling
    assertthat::assert_that(namesIn())
    
    if (substr(markedCols[1], 1, 1) == "-") {
        # Delete these   
    } else {
        # Keep these
    }
    
    # Have vector of columns to use
}

#' keepColsDT()
#'
#' Deletes all of the columns in a data.table by reference except for the 
#' columns that you specify to keep.
#' 
#' @keywords keep, columns, DT, data.table
#' @param data a data.table
#' @param keepCols a character vector of column names NOT to delete
#' @param slient FALSE by default, if TRUE it turns off the messages that
#' appear after each column is deleted by reference.
#' @export
#' @examples
#' 
#' columnsToKeep <- c("mpg", "cyl", "disp")
#' mtcars <- data.table(mtcars)
#' keepColsDT(mtcars, columnsToKeep)

keepColsDT <- function(data = NULL, keepCols = NULL, silent = FALSE) {
    
    # Error handling
    assertthat::assert_that(notNULL(data))
    assertthat::assert_that(notNULL(keepCols))
    assertthat::assert_that(is.DT(data))
    assertthat::assert_that(namesIn(keepCols, data))
    
    # Figure out which columns to delete
    allCols <- names(data)
    deleteCols <- allCols[!(allCols %in% keepCols)]
    if (length(deleteCols) < 1) {
        stop("No columns found to delete, please check your keepCols arg.")
    }
    
    # Delete the columns
    for (col in deleteCols) set(data, , col, NULL)
    
    # Messages after each column is deleted
    if (!silent & interactive()) {
        message(paste(deleteCols, collapse = ", "), " deleted by reference.")
    }
    
    invisible(data)
}

#' deleteColsDT()
#'
#' Deletes all of the specified columns in a data.table by reference.
#' 
#' @keywords delete, columns, DT, data.table
#' @param data a data.table
#' @param deleteCols a character vector of column names o delete
#' @param slient FALSE by default, if TRUE it turns off the messages that
#' appear after each column is deleted by reference.
#' @export
#' @examples
#' 
#' columnsToKeep <- c("mpg", "cyl", "disp")
#' mtcars <- data.table(mtcars)
#' keepColsDT(mtcars, columnsToKeep)

deleteColsDT <- function(data = NULL, deleteCols = NULL, silent = FALSE) {
    
    # Error handling
    assertthat::assert_that(notNULL(data))
    assertthat::assert_that(notNULL(deleteCols))
    assertthat::assert_that(is.DT(data))
    assertthat::assert_that(assertthat::not_empty(deleteCols))
    assertthat::assert_that(namesIn(deleteCols, data))
    
    # Delete the columns
    for (col in deleteCols) set(data, , col, NULL)
    
    # Messages after each column is deleted
    if (!silent & interactive()) {
        message(paste(deleteCols, collapse = ", "), " deleted by reference.")
    }
    
    invisible(data)
}

mergeDT <- function(DT1 = NULL, DT2 = NULL, keys = NULL, keepCols = NULL, 
                    deleteCols = NULL, silent = FALSE) {
    
    # Arguments: data.tables 1 (DT1) and 2 (DT2), a character vector of keys
    #            (keys) to be used to merge DT1 and DT2, and, optionally, 
    #            either a character vector of columns to keep from DT2 
    #            (keepCols) or remove from DT2 (removeCols)
    # Outputs: a merged data.table (full join)
    
    # Error handling - are DT1 and DT2 valid?
    assertthat::assert_that(notNULL(DT1))
    assertthat::assert_that(notNULL(DT2))
    assertthat::assert_that(is.DT(DT1))
    assertthat::assert_that(is.DT(DT2))
    
    # Copy the data.tables
    DT1 <- data.table::copy(DT1)
    DT2 <- data.table::copy(DT2)
    
    # Error handling - do DT1 and DT2 have identical keys? If not, the keys
    # argument must be supplied
    
    DT1keys <- attributes(DT1)$sorted
    DT2keys <- attributes(DT2)$sorted
    
    if (is.null(keys) & !is.null(DT1keys) & !is.null(DT2keys)) {
        if (identical(DT1keys, DT2keys)) {
            
        } else {
            stop("You have not specified the 'keys' argument, and your ",
                 "two data.tables do not have identical keys. Please either ",
                 "set the keys argument, or give your data.tables identical ",
                 "keys.")
        }
        
    } else {
        
        # Error handling - is the 'keys' argument supplied?
        if(is.null(keys)) {
            stop("You have not specified the 'keys' argument, and your ",
                 "two data.tables do not have identical keys. Please either ",
                 "set the keys argument, or give your data.tables identical ",
                 "keys.")
        }
        
        # Set appropriate keys
        data.table::setkeyv(DT1, keys)
        data.table::setkeyv(DT2, keys)
    }
    
    # Get a vector of columns to possibly delete from DT2
    allCols <- names(DT2)
    if (!is.null(keepCols) & !is.null(removeCols)) {
        deleteCols <- NULL
    } else if (!is.null(keepCols)) {
        keepColsDT(DT2, keepCols, silent = TRUE)
    } else if (!is.null(deleteCols)) {
        deleteColsDT(DT2, deleteCols, silent = TRUE)
    }
    
    # Merge the tables (full join)
    DT1[DT2]
}