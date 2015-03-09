#' longToDelimited()
#'
#' Turns long data into delimited data. 
#' 
#' Ex., imagine you have a table with 2 columns, the first is an Id column 
#' with "A" repeated 3 times, the second  column has rows X then Y then Z. 
#' This function would turn this table from having three rows into having one 
#' row where X, Y, Z are delimited by some delimiter (the default is ", " 
#' like the above example).
#' 
#' @keywords create, delimited, table, long
#' @param data a \code{data.frame} or \code{data.table} 
#' @param delimitThis the column that you want to become a delimited field
#' @param byThis the Id column which seperates different delimited sections
#' @param delimiter which delimiter do you want to use? Default is ", "
#' @param filter a data.table \code{i} expression (roughly equivalent to dplyr's
#' filter function) to filter the data before creating delimited fields
#' @param returnDT NULL by default, can set to TRUE or FALSE to explicitly 
#' return a data.table. By default, a data.table is returned if table is a
#' data.table, and a data.frame is returned if table is a data.frame.
#' @param keepCols a vector of column names from \code{data} to keep in the 
#' final table. Keep in mind that only the first column corresponding with 
#' each \code{byThis} will be kept. Using \code{keepCols} is really only useful
#' when the columns you are interested in have identical rows for each unique
#' value of \code{byThis}.
#' @param allCols FALSE by default, if TRUE, sets keepCols to every column 
#' except for the delimitThis and byThis columns
#' @export
#' @examples
#' 
#' \code{delimited <- longToDelimited(myData, "soccerPlayers", "team")}

longToDelimited <- function(data, delimitThis, byThis, delimiter = ", ",
                            filter = NULL, returnDT = NULL,
                            keepCols = NULL, allCols = FALSE) {
    
    ## Error handling -----------------------------------------------------------
    
    # If arguments are missing
    if (is.null(data)) stop("Requires argument for data")
    if (is.null(delimitThis)) stop("Requires argument for delimitThis")
    if (is.null(byThis)) stop("Requires argument for byThis")
    
    # If data isn't a data.frame, get outta here
    if (!(is.data.frame(data))) {
        stop("The data argument must be a data.frame (or data.table), or ",
             "inherit data.frame")
    }
    
    ## Create the delimited data from the long data -----------------------------
    
    # If data is a data.table, copy it, if not don't
    if (inherits(data, "data.table")) {
        myTable <- data.table::copy(data)
    } else {
        myTable <- data.table(data)
    }
    
    # Get table names
    tableNames <- names(data)
    
    # If FILTER isn't NULL, run the filter
    if (!is.null(filter)) {
        filterFun <- parse(text = paste0(filter))
        myTable <- myTable[eval(filterFun)]
    }
    
    # If keepAll is TRUE, change keepCols to all columns except for longThis 
    # and byThis
    if (allCols) {
        if (!is.null(keepCols)) message("keepCols being overwritten as you ",
                                        "specified allCols to be TRUE")
        keepCols <- tableNames[!tableNames %in% c(longThis, byThis)]
    }
    
    # Delete all unnecessary columns
    deleteThese <- tableNames[!tableNames %in% c(delimitThis, byThis, 
                                                 keepCols)]
    myTable[, c(deleteThese) := NULL]
    
    # Change column ordering, so order is byThis, longThis, ...
    data.table::setcolorder(myTable, c(byThis, delimitThis, keepCols))
    
    # Create the delimited field
    evalFun <- parse(text = paste0("Delimit := paste(", delimitThis,
                                   ', collapse = "', delimiter, '")'))
    myTable[, eval(evalFun), by = byThis]
    
    # Remove duplicate byThis
    dupCol <- parse(text = paste0("!duplicated(", byThis, ")"))
    myTable <- myTable[eval(dupCol)]
    
    # Add correct column names
    myTable[, c(delimitThis) := NULL]
    data.table::setnames(myTable, c(byThis, delimitThis, keepCols))
    
    ## Return the new data ------------------------------------------------------
    
    # Set returnDT to 0 if NULL
    if (is.null(returnDT)) returnDT <- "x"
    
    # Return the table
    if (returnDT == TRUE) {
        return(myTable)
        
    } else if (returnDT == FALSE) {
        return(as.data.frame(myTable))
        
    } else if (inherits(data, "data.table")) {
        return(myTable)
        
    } else {
        return(as.data.frame(myTable))
    }
}

#' delimitedToLong()
#'
#' Turns delimited data into long form data. 
#' 
#' Ex., imagine you have a table with 2 columns, the first is an Id column 
#' with "A" repeated 3 times, the second  column has rows X then Y then Z. 
#' This function would turn this table from having three rows into having one 
#' row where X, Y, Z are delimited by some delimiter (the default is ", " 
#' like the above example).
#' 
#' @keywords create, long, table, delimited
#' @param data a \code{data.frame} or \code{data.table} 
#' @param longThis the column that you want to become a delimited field
#' @param byThis the Id column which seperates different delimited sections
#' @param delimiter which delimiter do you want to use? Default is ", "
#' @param filter a data.table \code{i} expression (roughly equivalent to dplyr's
#' filter function) to filter the data before creating delimited fields
#' @param returnDT NULL by default, can set to TRUE or FALSE to explicitly 
#' return a data.table. By default, a data.table is returned if table is a
#' data.table, and a data.frame is returned if table is a data.frame.
#' @param keepCols a vector of column names from \code{data} to keep in the 
#' final table. Keep in mind that only the first column corresponding with 
#' each \code{byThis} will be kept. Using \code{keepCols} is really only useful
#' when the columns you are interested in have identical rows for each unique
#' value of \code{byThis}.
#' @param allCols FALSE by default, if TRUE, sets keepCols to every column 
#' except for the toThis and byThis columns
#' @param longThisClass optional, the class that you want the column of longThis
#' to be in your final data. All delimited fields are going to be character 
#' vectors, as they include character delimiters, so the resulting longThis
#' column is going to be of the class character. You may specify a different 
#' class here, ex. factor or numeric. 
#' @param ... additional arguments to the \code{data.table::merge} function if
#' you are either specifying keepCols, or setting allCols to be TRUE. By default,
#' no arguments are given to \code{data.table::merge} except for \code{x}, 
#' \code{y}, and \code{by}.
#' @export

delimitedToLong <- function(data, longThis, byThis, delimiter = ", ",
                            filter = NULL, returnDT = NULL,
                            keepCols = NULL, allCols = FALSE, 
                            longThisClass = NULL, ...) {
    
    ## Error handling -----------------------------------------------------------
    
    # If arguments are missing
    if (is.null(data)) stop("Requires argument for data")
    if (is.null(delimitThis)) stop("Requires argument for delimitThis")
    if (is.null(byThis)) stop("Requires argument for byThis")
    
    # If data isn't a data.frame, get outta here
    if (!(is.data.frame(data))) {
        stop("The data argument must be a data.frame (or data.table), or ",
             "inherit data.frame")
    }
    
    ## Create the long data from the delimited data -----------------------------
    
    # If data is a data.table, copy it, if not don't
    if (inherits(data, "data.table")) {
        myTable <- data.table::copy(data)
    } else {
        myTable <- data.table(data)
    }
    
    # Get table names
    tableNames <- names(data)
    
    # If FILTER isn't NULL, run the filter
    if (!is.null(filter)) {
        filterFun <- parse(text = paste0(filter))
        myTable <- myTable[eval(filterFun)]
    }
    
    # If keepAll is TRUE, change keepCols to all columns except for longThis 
    # and byThis
    if (allCols) {
        if (!is.null(keepCols)) message("keepCols being overwritten as you ",
                                        "specified allCols to be TRUE")
        keepCols <- tableNames[!tableNames %in% c(longThis, byThis)]
    }
    
    # Delete all unnecessary columns
    deleteThese <- tableNames[!tableNames %in% c(longThis, byThis)]
    if (length(deleteThese) != 0) myTable[, c(deleteThese) := NULL]
    
    # Change column ordering, so order is byThis, longThis
    data.table::setcolorder(myTable, c(byThis, longThis))
    
    # Create the delimited field
    evalFun <- parse(text = paste0(longThis, " := strsplit(", longThis,
                                   ", '", delimiter, "')"))
    myTable[, eval(evalFun)]
    
    # Remove duplicate byThis
    evalFun <- parse(text = paste0("unlist(", longThis, ")"))
    myTable <- myTable[, eval(evalFun), by = byThis]
    
    # Add correct column names
    data.table::setnames(myTable, c(byThis, longThis))
    
    # Change the class of longThis if requested
    if (!is.null(longThisClass)) {
        changeColClass(myTable, class(myTable$longThis), longThisClass,
                       onlyConvert = longThis)
    }
    
    # If keepCols isn't NULL, join those columns on the new data
    if (!is.null(keepCols)) {
        myTable <- merge(myTable, data, by = byThis, ...)
    }
    
    ## Return the new data ------------------------------------------------------
    
    # Set returnDT to 0 if NULL
    if (is.null(returnDT)) returnDT <- "x"
    
    # Return the table
    if (returnDT == TRUE) {
        return(myTable)
        
    } else if (returnDT == FALSE) {
        return(as.data.frame(myTable))
        
    } else if (inherits(data, "data.table")) {
        return(myTable)
        
    } else {
        return(as.data.frame(myTable))
    }
}