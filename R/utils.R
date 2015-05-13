## =============================================================================
##
## Internal functions to the jakemisc package. These functions are not exported,
## therefore if you are trying to use one you need to use the ::: operator. 
## Functions from within the package can access these internal functions via 
## the package's namespace. 
##
## =============================================================================

# compatibility for data.table functions
# (see: https://github.com/hadley/dplyr/issues/548)
.datatable.aware <- TRUE

# Function for getting a numerical vector of the positions of a pattern
# within a string (using regex, from the grepexpr function)
patternPositions <- function(string, pattern, ignore.case = FALSE) {
    charPositions <- gregexpr(pattern, string, ignore.case)
    charPositions <- charPositions[[1]][1:length(charPositions[[1]])]
    return(charPositions)
}

# Get vector of names of all installed packages
installedPackages <- function() {
    all_packages <- installed.packages()
    return(all_packages[1:(length(all_packages)/16)])
}

# Is a package installed?
isPackageInstalled <- function(packageName) {
    return(packageName %in% installedPackages())
}

# Turn one or more NSE names (...) into a character vector, removing " marks
NSEtoVector <- function(..., USE.NAMES = FALSE) {
    
    # Concatenate the package names together via non-standard evaluation
    # (NSE) so quotes do not need to be placed around package names
    package_names <- sapply(substitute(...()), deparse)
    
    # Remove escaped quotation marks if the the user inputted any package 
    # names in quotation marks:
    package_names <- sapply(package_names, function(f) {
        if (grepl("\"", f)) f <- gsub("\"", "", f)
        return(f)
    }, USE.NAMES = USE.NAMES)
    
    return(package_names)
}

# Given a string a symbol, return the position of the symbol w/i the string
getSymbolPosition <- function(string, symbol) {
    return(gregexpr(symbol, string)[[1]][1])
}

# Seperate a character vector of length 1 based on an inputted symbol
seperateSymbol <- function(string, symbol) {
    symbolPosition <- getSymbolPosition(string, symbol)
    before_symbol <- substr(string, 1, (symbolPosition - 1))
    after_symbol <- substr(string, (symbolPosition + 1), 
                   nchar(string))
    return(c(before_symbol, after_symbol))
}

# Return the column order of a subset of columns from a data.frame
subsetColOrder <- function(data, subset_cols) {
    subset_order <- sapply(subset_cols, grep, colnames(data), 
                           USE.NAMES = FALSE)
    return(order(subset_order))
}

# Detect if a numeric object is a whole number or not
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
    abs(x - round(x)) < tol
}

# Function for summing NAs
sumNAs <- function(f) sum(is.na(f))

# dplyr like column selection
columnsToOperateOn <- function(allCols, markedCols) {
    
    # Arguments: a character vector of all column names (allCols), and a 
    #            character vector of "marked" column names (markedCols) where
    #            some columns may be preceded w/ a "-" to indicate that we
    #            want all columns but those ones
    # Outputs: a character vector of columns from allCols (but decided by
    #          markedCols to use)
    
    # Error handling
    assertthat(all(markedCols %in% allCols))
    
    # Does the first column contain a "-"? Return TRUE if so.
    firstColMinus <- function(markedCols) {
        firstCol <- FALSE
        minusCols <- substr(markedCols, 1, 1) == "-"
        if (minusCols[1]) firstCol <- TRUE
        firstCol
    }
    
    # Function for getting all minus columns in markedCols
    minusCols <- function(markedCols) {
        markedCols[substr(markedCols, 1, 1) == "-"]
    }
    
    # Function for geting all non-minus columns in markedCols
    nonMinusCols <- function(markedCols) {
        markedCols[substr(markedCols, 1, 1) != "-"]
    }
    
    # Get minusCols and markedCols
    localMinusCols <- minusCols(markedCols)
    localNonMinusCols <- nonMinusCols(markedCols)
    
    # Are there any duplicates in either of the above?
    if (!is.null())
    
    # If any columns overlap between the two, throw an error
    if (!is.null(localMinusCols) & !is.null(localNonMinusCols) &
        any(localMinusCols %in% localNonMinusCols)) {
        stop("You cannot specify columns as both minus and nonMinus")
    }
    
    # If the first column in markedCols contains a "-", delete all columns in
    # markedCols from allCols that contain a "-". Otherwise, keep all columns
    # in markedCols that do not contain a "-".
    if (substr(markedCols[1], 1, 1) == "-") {
        # Delete these   
    } else {
        # Keep these
    }
    
    # Have vector of columns to use
}