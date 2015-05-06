## ============================================================================
##
## Functions specific to the data.table package
##
## Includes 1. data.table related functions not for export
##          2. functions for export
##
## Assertations related to data.tables are in assertations.R
##
## ============================================================================

## data.table related functions not for export --------------------------------

# Turns a vector of numerics or characters into essentially a NSE
# version, where the input to the expression is returned, unevaluted
vectorToParsedTxt <- function(vector, wrap = '"', collapse = ", ",
                              concatenate = "c(", end = ")") {
    vector %>%
        paste0(wrap, ., wrap) %>%
        paste(collapse = collapse) %>%
        paste0(concatenate, ., end)
}

# Returns vectorToParsedTxt for a list of vectors. If the vector is
# numeric, nothing is wrapped around the numbers, otherwise single quotes
# are wrapped around the elements of each list/vector
vectorsToParsedTxt <- function(listVectors) {
    lapply(listVectors, function(f) {
        if (is.numeric(f)) {
            vectorToParsedTxt(f, wrap = "") 
        } else if (is.factor(f)) {
            vectorToParsedTxt(f, concatenate = "factor(c(", end = "))")
        } else {
            vectorToParsedTxt(f) 
        }
    })
}

# Turns a list of vectors into an unevaluated i expression in data.table
# that will subset the data.table by the expression (note that keys must
# already be set on the data.table for this to work)
vectorsToParsedTxtDT <- function(listVectorsNSE) {
    listVectorsNSE %>%
        vectorsToParsedTxt() %>%
        unlist() %>%
        paste(collapse = ", ") %>%
        paste0("list(", .,")")
}

# Function to create an S3 object: subsetObj
subsetObj <- function(...) {
    
    # Put ... into a list
    obj <- list(...)
    
    # Error handling - is the length of ... > 0
    assertthat::assert_that(length(obj) > 0)
    
    # Error handling - does every element have a name?
    if ("" %in% names(obj)) {
        stop("Each element of the subsetObj needs to have a name that ",
             "corresponds with a column name.")
    }
    
    # Error handling - does each element have > 0 elements w/i?
    if (all(vapply(obj, length, numeric(1)) == 0)) {
        stop("Each element needs to have one or more items to subset on.")
    }
    
    structure(obj, class = "subsetObj")
}

# Function for determining if an object is a subsetObj
is.subsetObj <- function(obj) inherits(obj, "subsetObj")

# Function for extracting column names from a subsetObj
getSubsetCols <- function(subsetObj) {
    UseMethod("getSubsetCols")
}

getSubsetCols.subsetObj <- function(subsetObj) {
    names(subsetObj)
}

# Function for getting a data.table expression for a subsetObj
getSubsetExpr <- function(subsetObj) {
    UseMethod("getSubsetExpr")
}

getSubsetExpr.subsetObj <- function(subsetObj) {
    vectorsToParsedTxtDT(subsetObj)
}

# keyMatch: determines if the keys in a data.table exactly match the a 
# character vector of specified keys
keyMatch <- function(data, keys) {
    UseMethod("keyMatch")
}

keyMatch.data.table <- function(data, keys) {
    dataKeys <- attributes(data)$sorted
    ifelse(is.null(dataKeys), FALSE, identical(dataKeys, keys))
}

# hasKey: TRUE or FALSE, does a data.table have a key set?
hasKey <- function(data) {
    UseMethod("hasKey")
}

hasKey.data.table <- function(data) {
    !is.null(attributes(data)$sorted)
}

## Functions for export -------------------------------------------------------

#' keepColsDT()
#'
#' Deletes all of the columns in a data.table by reference except for the 
#' columns that you specify to keep.
#' 
#' @keywords keep, columns, DT, data.table
#' @param data a data.table
#' @param keepCols a character vector of column names NOT to delete
#' @param silent FALSE by default, if TRUE it turns off the messages that
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
    #assertthat::assert_that(namesIn(keepCols, data))
    if (!namesIn(keepCols, data)) {
        stop("One or more names in 'keepCols' is not present in names(data).")
    }
    
    # Figure out which columns to delete
    allCols <- names(data)
    deleteCols <- allCols[!(allCols %in% keepCols)]
    if (length(deleteCols) < 1) {
        stop("No columns found to delete, please check your keepCols arg.")
    }
    if (identical(deleteCols, allCols)) {
        stop("You cannot delete every remaining column in your 'data' arg.")
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
#' @param silent FALSE by default, if TRUE it turns off the messages that
#' appear after each column is deleted by reference.
#' @export
#' @examples
#' 
#' columnsToDelete <- c("mpg", "cyl", "disp")
#' mtcars <- data.table(mtcars)
#' deleteColsDT(mtcars, columnsToDelete)

deleteColsDT <- function(data = NULL, deleteCols = NULL, silent = FALSE) {
    
    # Error handling
    assertthat::assert_that(notNULL(data))
    assertthat::assert_that(notNULL(deleteCols))
    assertthat::assert_that(is.DT(data))
    assertthat::assert_that(assertthat::not_empty(deleteCols))
    #assertthat::assert_that(namesIn(deleteCols, data))
    if (!namesIn(deleteCols, data)) {
        stop("One or more names in 'keepCols' is not present in names(data).")
    }
    if (identical(deleteCols, names(data))) {
        stop("You cannot delete every remaining column in your 'data' arg.")
    }
    
    # Delete the columns
    for (col in deleteCols) set(data, , col, NULL)
    
    # Messages after each column is deleted
    if (!silent & interactive()) {
        message(paste(deleteCols, collapse = ", "), " deleted by reference.")
    }
    
    invisible(data)
}

#' mergeDT()
#'
#' Merges two data.tables using 
#' 
#' @keywords merge, tables, DT, data.table, fast, key, setkey
#' @param DT1 a data.table
#' @param DT2 a data.table
#' @param keys a character vector of one or more columns to join on between
#' DT1 or DT2. If you have already used \code{data.table::setkey()} to set
#' the keys for DT1 and DT2 and those keys are identical, and you do not
#' specify the keys argument, the join will be performed on those columns.
#' @param keepCols a character vector of column names NOT to delete. You may
#' specify keepCols or deleteCols but NOT BOTH.
#' @param deleteCols a character vector of column names to delete. You may
#' specify keepCols or deleteCols but NOT BOTH.
#' @export
#' @examples
#' 
#' Merging two data.tables where we specify the key in \code{mergeDT}:
#' mergeDT(DT1, DT2, keys = "common_id")
#' 
#' Setting keys first, then merging two data.tables:
#' setkey(DT1, common_id)
#' setkey(DT2, common_id)
#' mergeDT(DT1, DT2)

mergeDT <- function(DT1 = NULL, DT2 = NULL, keys = NULL, keepCols = NULL, 
                    deleteCols = NULL) {
    
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
    
    # Keep or delete columns depending on the keepCols and deleteCols args
    allCols <- names(DT2)
    if (!is.null(keepCols) & !is.null(deleteCols)) {
        stop("You can only specify one of 'keepCols' or 'deleteCols'")
        
    } else if (!is.null(keepCols)) {
        keepColsDT(DT2, keepCols, silent = TRUE)
        
    } else if (!is.null(deleteCols)) {
        deleteColsDT(DT2, deleteCols, silent = TRUE)
    }
    
    # Merge the tables (full join)
    DT1[DT2]
}

#' subsetDT()
#'
#' Merges two data.tables using 
#' 
#' @keywords subset, rows, DT, data.table
#' @param data a data.table
#' @param ... one or more lists, where the first element of the list is the
#' column name, and the second element is one or more items from that column
#' to subset. 
#' @param ref TRUE by default, it will take the keys that are set during 
#' the subset process and apply those keys to data by reference. If FALSE,
#' a copy of data is made via \code{data.table::copy()}, leaving data
#' unaffected by the key setting in subsetDT.
#' @export

subsetDT <- function(data, ..., ref = TRUE) {
    
    # Error handling
    assertthat::assert_that(notNULL(data))
    assertthat::assert_that(is.DT(data))
    assertthat::assert_that(is.flag(ref))
    
    # Create a subset object if one wasn't passed to ...
    obj <- subsetObj(...)
    columns <- getSubsetCols(obj)
    
    # Are all of the columns in names(data)?
    if (!namesIn(columns, data)) {
        stop("One or more of your columns are not present in names(data).")
    }
    
    # If keys matching the columns in ... exist, use those keys. If not, set
    # keys. If ref == FALSE, make a data.table::copy of data, otherwise the
    # keys will be set by reference.
    if (!keyMatch(data, columns)) {
        if (ref == FALSE) data <- data.table::copy(data)
        setkeyv(data, columns)
    }

    # Get the expression
    subsetExpr <- getSubsetExpr(obj)
    
    # Subset data
    data[eval(parse(text = subsetExpr)), ]
}