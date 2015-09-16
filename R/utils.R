## ============================================================================
##
## Internal functions to the jakemisc package. These functions are not exported
## , therefore if you are trying to use one you need to use the ::: operator. 
## Functions from within the package can access these internal functions via 
## the package's namespace. 
##
## ============================================================================

# Save typing by importing functions from other packages::
##
#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @importFrom magrittr "%T>%"
#' @importFrom magrittr "%$%"
#' @importFrom assertthat assert_that
#' @importFrom assertthat is.flag
#' @importFrom assertthat not_empty

# compatibility for data.table functions
# (see: https://github.com/hadley/dplyr/issues/548)
.datatable.aware <- TRUE

# Return TRUE if all ... have the same length
equal_length <- function(...) {
    objects <- list(...)
    objects_range <- range(vapply(objects, length, numeric(1)))
    objects_range[1] == objects_range[[2]]
}

named_list <- function(...) {
    
    # Returns a list of objects in ... but uses NSE to name each element of 
    # the list as the name of that object
    list_names <- sapply(substitute(...()), deparse)
    named_list <- list(...)
    names(named_list) <- list_names
    
    named_list
}

# Fast version of the %in% operator
`%fin%` <- function(x, table) fastmatch::fmatch(x, table, nomatch = 0L) > 0L

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
