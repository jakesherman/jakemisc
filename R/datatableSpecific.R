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

is.data.table <- function(x) inherits(x, "data.table")

assertthat::on_failure(is.data.table) <- function(call, env) {
    paste0(deparse(call$x), " is not a data.table.")
}

namesIn <- function(names, df) {
    
    # Arguments: character vector (names), a data.frame (df) w/ names
    # Outputs: are all of names in names(df)? TRUE or FALSE.
    
    all(names %in% names(df))
}

assertthat::on_failure(is.data.table) <- function(call, env) {
    paste0("One or more of the names in '", deparse(call$x), "' is not a name",
           " in the data.frame")
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
    assertthat::assert_that(is.data.table(data))
    assertthat::assert_that(assertthat::not_empty(keepCols))
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
    
    return(invisible(data))
}

deleteColsDT <- function(data, deleteCols, silent = FALSE) {
    
    # Error handling
    assertthat::assert_that(notNULL(data))
    assertthat::assert_that(notNULL(deleteCols))
    assertthat::assert_that(is.data.table(data))
    assertthat::assert_that(assertthat::not_empty(deleteCols))
    assertthat::assert_that(namesIn(deleteCols, data))
    
    # Delete the columns
    for (col in deleteCols) set(data, , col, NULL)
    
    # Messages after each column is deleted
    if (!silent & interactive()) {
        message(paste(deleteCols, collapse = ", "), " deleted by reference.")
    }
    
    return(invisible(data))
}