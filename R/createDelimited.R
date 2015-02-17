#' createDelimited()
#'
#' Turns long data into delimited data. Ex., imagine you have a table with 2
#' columns, the first is an Id column with "A" repeated 3 times, the second 
#' column has rows X then Y then Z. This function would turn this table from
#' having three rows into having one row where X, Y, Z are delimited by
#' some delimiter (the default is ", " like the above example).
#' 
#' @keywords matchup, match, up, lookup, crosswalk
#' @param table a data.frame or data.table 
#' @param delimit_this the column that you want to become a delimited field
#' @param by_this the Id column which seperates different delimited sections
#' @param delimiter which delimiter do you want to use? Default is ", "
#' @param filter a data.table i expression (roughly equivalent to dplyr's
#' filter function) to filter the data before creating delimited fields
#' @param returnDT NULL by default, can set to TRUE or FALSE to explicitly 
#' return a data.table. By default, a data.table is returned if table is a
#' data.table, and a data.frame is returned if table is a data.frame.
#' @export

# Function for delimited tables
createDelimited <- function(table, delimit_this, by_this, delimiter = ", ",
                            filter = NULL, returnDT = NULL,
                            keepCols = NULL) {
    
    # If table is a data.table, copy it, if not don't
    if (inherits(table, "data.table")) {
        myTable <- copy(table)
    } else {
        myTable <- data.table(table)
    }
    
    # Get table names
    tableNames <- names(table)
    
    # If FILTER isn't NULL, run the filter
    if (!is.null(filter)) {
        filterFun <- parse(text = paste0(filter))
        myTable <- myTable[eval(filterFun)]
    }
    
    # Delete all unnecessary columns
    deleteThese <- tableNames[!tableNames %in% c(delimit_this, by_this, 
                                                 keepCols)]
    myTable[, c(deleteThese) := NULL]
    
    # Create the delimited field
    evalFun <- parse(text = paste0("Delimit := paste(", delimit_this,
                                   ', collapse = "', delimiter, '")'))
    myTable[, eval(evalFun), by = by_this]
    
    # Remove duplicate by_this
    dupCol <- parse(text = paste0("!duplicated(", by_this, ")"))
    myTable <- myTable[eval(dupCol)]
    
    # Add correct column names
    myTable[, c(delimit_this) := NULL]
    setnames(myTable, c(by_this, delimit_this))
    
    # Set returnDT to 0 if NULL
    if (is.null(returnDT)) returnDT <- 0
    
    # Return the table
    if (returnDT == TRUE) {
        return(myTable)
        
    } else if (returnDT == FALSE) {
        return(as.data.frame(myTable))
        
    } else if (inherits(table, "data.table")) {
        return(myTable)
        
    } else {
        return(as.data.frame(myTable))
    }
}