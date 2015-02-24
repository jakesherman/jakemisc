#' createDelimited()
#'
#' Turns long data into delimited data. 
#' 
#' Ex., imagine you have a table with 2 columns, the first is an Id column 
#' with "A" repeated 3 times, the second  column has rows X then Y then Z. 
#' This function would turn this table from having three rows into having one 
#' row where X, Y, Z are delimited by some delimiter (the default is ", " 
#' like the above example).
#' 
#' @keywords create, delimited, table
#' @param data a \code{data.frame} or \code{data.table} 
#' @param delimitThis the column that you want to become a delimited field
#' @param byThis the Id column which seperates different delimited sections
#' @param delimiter which delimiter do you want to use? Default is ", "
#' @param filter a data.table i expression (roughly equivalent to dplyr's
#' filter function) to filter the data before creating delimited fields
#' @param returnDT NULL by default, can set to TRUE or FALSE to explicitly 
#' return a data.table. By default, a data.table is returned if table is a
#' data.table, and a data.frame is returned if table is a data.frame.
#' @param keepCols a vector of column names from \code{data} to keep in the 
#' final table. Keep in mind that only the first column corresponding with 
#' each \code{byThis} will be kept. Using \code{keepCols} is really only useful
#' when the columns you are interested in have identical rows for each unique
#' value of \code{byThis}.
#' @param invisible TRUE (by default), invisibly return the data?
#' @export
#' @examples
#' 
#' delimited <- createDelimited(myData, "soccerPlayers")

# Function for delimited tables
createDelimited <- function(data, delimitThis, byThis, delimiter = ", ",
                            filter = NULL, returnDT = NULL,
                            keepCols = NULL, invisible = TRUE) {
    
    # If data is a data.table, copy it, if not don't
    if (inherits(data, "data.table")) {
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
    deleteThese <- tableNames[!tableNames %in% c(delimitThis, byThis, 
                                                 keepCols)]
    myTable[, c(deleteThese) := NULL]
    
    # Create the delimited field
    evalFun <- parse(text = paste0("Delimit := paste(", delimitThis,
                                   ', collapse = "', delimiter, '")'))
    myTable[, eval(evalFun), by = byThis]
    
    # Remove duplicate byThis
    dupCol <- parse(text = paste0("!duplicated(", byThis, ")"))
    myTable <- myTable[eval(dupCol)]
    
    # Add correct column names
    myTable[, c(delimitThis) := NULL]
    setnames(myTable, c(byThis, delimitThis))
    
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
    
    # Return the data
    if (invisible == TRUE) {
        
        # Return the table invisibly
        if (returnDT == TRUE) {
            return(invisible(myTable))
            
        } else if (returnDT == FALSE) {
            return(invisible(as.data.frame(myTable)))
            
        } else if (inherits(table, "data.table")) {
            return(invisible(myTable))
            
        } else {
            return(invisible(as.data.frame(myTable)))
        }
        
    } else {
        
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
}