## =============================================================================
##
## Internal functions to the jakemisc package. These functions are not exported,
## therefore if you are trying to use one you need to use the ::: operator. 
## Functions from within the package can access these internal functions via 
## the package's namespace. 
##
## =============================================================================

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