## ============================================================================
##
## Various NAs functions - contains:
##     1) NAs()
##     2) NaNs()
##     3) NonNAs()
##
## ============================================================================

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
#' To see the number of NAs either in total or by column, depending on the 
#' class of the object inputted:
#' NAs(my_data)
#' 
#' Same as above, but now looking at the percent of NAs:
#' NAs(my_data, type = "percent")
 
NAs <- function(data, type, digits) {
    UseMethod("NAs")
}

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
        NAs <- round(sumNAs(data) / length(data), digits = digits)
        
    } else {
        stop("Invalid type argument")
    }
    
    return(NAs)
}

#' NaNs()
#'
#' Returns the number of NaNs in an object by column (for data.frames and 
#' matricies) or just the number of NaNs (for all other objects). You may also
#' set the type argument to \code{"percent"} to see the percent of NAs.
#' 
#' @keywords NaN, NaNs, percent, valid, missing, data
#' @param data an object, can be a data.frame (see NaNs by column), matrix 
#' (also see NaNs by column), or a vector
#' @param type \code{"number"}  is the default, displaying the number of NaNs. 
#' You may also use \code{"percent"}  to see the percent of NaNs
#' @param digits the number of digits to round to when the type arguemnt is set
#' to \code{"percent"} 
#' @export
#' @examples
#' 
#' To see the number of NaNs either in total or by column, depending on the 
#' class of the object inputted:
#' NaNs(my_data)
#' 
#' Same as above, but now looking at the percent of NAs:
#' NaNs(my_data, type = "percent")

NaNs <- function(data, type, digits) {
    UseMethod("NaNs")
}

# Not for export - function for summing NaNs
sumNaNs <- function(f) sum(is.nan(f))

#' @export
NaNs.data.frame <- function(data, type = "number", digits = 2) {
    
    # Find NaNs
    if (type == "number") {
        NaNs <- sapply(data, sumNaNs)
        
    } else if (type == "percent") {
        NaNs <- round(sapply(data, sumNaNs) / nrow(data), digits = digits)
        
    } else {
        stop("Invalid type argument")
    }
    
    return(NaNs)
}

#' @export
NaNs.matrix <- function(data, type = "number", digits = 2) {
    
    # Find NaNs
    if (type == "number") {
        NaNs <- apply(data, 2, sumNaNs)
        
    } else if (type == "percent") {
        NaNs <- round(apply(data, 2, sumNaNs) / nrow(data), digits = digits)
        
    } else {
        stop("Invalid type argument")
    }
    
    # Give the matrix column identifers if there are no column names
    if (is.null(colnames(data))) {
        names(NaNs) <- paste0("col", 1:ncol(data))
    }
    
    return(NaNs)
}

#' @export
NaNs.default <- function(data, type = "number", digits = 2) {
    
    # Find NaNs
    if (type == "number") {
        NaNs <- sumNaNs(data)
        
    } else if (type == "percent") {
        NaNs <- round(sumNaNs(data) / length(data), digits = digits)
        
    } else {
        stop("Invalid type argument")
    }
    
    return(NaNs)
}

#' NonNAs()
#'
#' Returns the number of Non-NAs in an object by column (for data.frames and 
#' matricies) or just the number of Non-NAs (for all other objects). You may 
#' also set the type argument to \code{"percent"} to see the percent of Non-NAs.
#' 
#' @keywords NA, NAs, percent, valid, missing, data, gone
#' @param data an object, can be a data.frame (see Non-NAs by column), matrix 
#' (also see Non-NAs by column), or a vector
#' @param type \code{"number"}  is the default, displaying the number of 
#' Non-NAs. You may also use \code{"percent"}  to see the percent of Non-NAs
#' @param digits the number of digits to round to when the type arguemnt is set
#' to \code{"percent"} 
#' @export
#' @examples
#' 
#' To see the number of Non-NAs either in total or by column, depending on the 
#' class of the object inputted:
#' NonNAs(my_data)
#' 
#' Same as above, but now looking at the percent of Non_NAs:
#' NonNAs(my_data, type = "percent")

NonNAs <- function(data, type, digits) {
    UseMethod("NonNAs")
}

# Not for export - function for summing NonNAs
sumNonNAs <- function(f) sum(!is.na(f))

#' @export
NonNAs.data.frame <- function(data, type = "number", digits = 2) {
    
    # Find NonNAs
    if (type == "number") {
        NonNAs <- sapply(data, sumNonNAs)
        
    } else if (type == "percent") {
        NonNAs <- round(sapply(data, sumNonNAs) / nrow(data), digits = digits)
        
    } else {
        stop("Invalid type argument")
    }
    
    return(NonNAs)
}

#' @export
NonNAs.matrix <- function(data, type = "number", digits = 2) {
    
    # Find NonNAs
    if (type == "number") {
        NonNAs <- apply(data, 2, sumNonNAs)
        
    } else if (type == "percent") {
        
        NonNAs <- round(apply(data, 2, sumNonNAs) / nrow(data), 
                        digits = digits)
        
    } else {
        stop("Invalid type argument")
    }
    
    # Give the matrix column identifers if there are no column names
    if (is.null(colnames(data))) {
        names(NonNAs) <- paste0("col", 1:ncol(data))
    }
    
    return(NonNAs)
}

#' @export
NonNAs.default <- function(data, type = "number", digits = 2) {
    
    # Find NonNAs
    if (type == "number") {
        NonNAs <- sumNonNAs(data)
        
    } else if (type == "percent") {
        NonNAs <- round(sumNonNAs(data) / length(data), digits = digits)
        
    } else {
        stop("Invalid type argument")
    }
    
    return(NonNAs)
}