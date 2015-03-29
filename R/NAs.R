#' NAs()
#'
#' Returns the number of NAs in an object by column (for data.frames and 
#' matricies) or just the number of NAs (for all other objects). You may also
#' set the type argument to \code{"percent"} to see the percent of NAs.
#' 
#' @keywords NA, NAs, percent, valid, missing, data, gone
#' @param data a \code{data.frame} or \code{data.table} 
#' @param type
#' @export
#' @examples
 
NAs <- function(data, type, digits) {
    UseMethod("NAs")
}

# Data.frame method, works with data.table and dplyr classes
#' @export
NAs.data.frame <- function(data, type = "number", digits = 2) {
    
    # Find NAs
    if (type == "number") {
        NAs <- sapply(data, function(f) sum(is.na(f)))
        
    } else if (type == "percent") {
        NAs <- round(sapply(data, function(f) sum(is.na(f))) / nrow(data), 
                     digits = digits)
        
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
        NAs <- apply(data, 2, function(f) sum(is.na(f)))
        
    } else if (type == "percent") {
        
        NAs <- round(apply(data, 2, function(f) sum(is.na(f))) / nrow(data), 
                     digits = digits)
        
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
        NAs <- sum(is.na(data))
        
    } else if (type == "percent") {
        NAs <- round(sum(is.na(data))/length(data), digits = digits)
        
    } else {
        stop("Invalid type argument")
    }
    
    return(NAs)
}