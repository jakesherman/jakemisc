#' NAs()
#'
#' Returns the number of NAs in an object by column (for data.frames and 
#' matricies) or just the number of NAs (for all other objects). You may also
#' set the type argument to \code{"percent"} to see the percent of NAs.
#' 
#' @keywords NA, NAs, percent, valid, missing, data, gone
#' @param data an object, can be a data.frame (see NAs by column), matrix (also
#' see NAs by column), or a vector
#' @param type \code{"number"}  is the default, displaying the number of NAs. 
#' You may also use \code{"percent"}  to see the percent of NAs
#' @param digits the number of digits to round to when the type arguemnt is set
#' to \code{"percent"} 
#' @export
#' @examples
#' 
#' To see the number of NAs either in total or by column, depending on the class
#' of the object inputted:
#' \code{NAs(my_data)}
#' 
#' Same as above, but now looking at the percent of NAs:
#' \code{NAs(my_data, type = "percent")}
 
NAs <- function(data, type, digits) {
    UseMethod("NAs")
}

# Not for export - function for summing NAs
sumNAs <- function(f) sum(is.na(f))

# Data.frame method, tested on data.table and dplyr table classes
#' @export
NAs.data.frame <- function(data, type = "number", digits = 2) {
    
    # Find NAs
    if (type == "number") {
        NAs <- sapply(data, sumNAs)
        
    } else if (type == "percent") {
        NAs <- round(sapply(data, sumNAs) / nrow(data), digits = digits)
        
    } else {
        stop("Invalid type argument")
    }
    
    return(NAs)
}

# Matrix method, uses apply instead of sapply and gives default column names
# if the colnames attribute is not present
#' @export
NAs.matrix <- function(data, type = "number", digits = 2) {
    
    # Find NAs
    if (type == "number") {
        NAs <- apply(data, 2, sumNAs)
        
    } else if (type == "percent") {
        
        NAs <- round(apply(data, 2, sumNAs) / nrow(data), digits = digits)
        
    } else {
        stop("Invalid type argument")
    }
    
    # Give the matrix column identifers if there are no column names
    if (is.null(colnames(data))) {
        names(NAs) <- paste0("col", 1:ncol(data))
    }
    
    return(NAs)
}

# Default method, for any type of vector
#' @export
NAs.default <- function(data, type = "number", digits = 2) {
    
    # Find NAs
    if (type == "number") {
        NAs <- sumNAs(data)
        
    } else if (type == "percent") {
        NAs <- round(sumNAs(data)/length(data), digits = digits)
        
    } else {
        stop("Invalid type argument")
    }
    
    return(NAs)
}