#' valuesToValue()
#'
#' Given a \code{data.frame} or \code{data.table}, turns selected values into 
#' a different value for specified columns. By default all columns are used, but 
#' you may specify the onlyConvert argument with a vector of one or more columns 
#' names and only do the conversion on these columns, or alternatively, specify 
#' the noConvert argument with a vector of one or more columns names that 
#' shouldnt have the conversion done on them. You may only choose to specify 
#' onlyConvert or noConvert, you may not specify both at the same time. 
#' 
#' This function uses seperate methods for data.tables and data.frames. By 
#' default, data.tables will be modified by reference. To turn off this
#' behavior, set \code{ref} to \code{FALSE}. 
#' 
#' @keywords valuesToValue, change, value, values
#' @param data a data frame/table that we want to remove NAs from
#' @param values a vector of one or more values that you wish to be converted
#' into another value
#' @param valueToConvertTo the value that you want values converted to
#' @param onlyConvert optional - specify a vector of one or more column names
#' as the only columns where the NA conversion will take place. You may either
#' have this parameter satified, or the noConvert parameter specified, you may
#' not specify both.
#' @param noConvert optional - specify a vector of one or more columns names 
#' that you do not want any NA conversion applied to. You may either have this 
#' parameter satified, or the noConvert parameter specified, you may not 
#' specify both.
#' @param ref TRUE (default) or FALSE, if TRUE and data is a data.table, modify 
#' the data.table by reference (modifying-in-place), if FALSE, do not modify
#' the data.table by reference, instead treat it like a data.frame (copy on
#' modify). 
#' @export
#' @examples
#' 
#' Lets say we want to convert "na" or -500 values into "jake" for only the
#' columns "town" and "city" in my_data:
#' \code{valuesToNA(my_data, c("na", -500), "jake", 
#'                  onlyConvert = c("town", "city"))}
#' 
#' Or, what if we want to convert "na" or -500 values into "jake" for every
#' columnn in my_data except for "town", "city", or "country":
#' \code{valuesToNA(my_data, c("na", -500), "jake", 
#'                  noConvert = c("town", "city", "country"))}

valuesToValue <- function(data = NULL, values = NULL, valueToConvertTo = NULL, 
                       onlyConvert = NULL, noConvert = NULL, ref = TRUE) {
    
    # NSE to get name of data
    data_name <- deparse(substitute(data))
    
    ## Error handling ----------------------------------------------------------
    
    # Missing arguments
    if (is.null(data)) stop("Please enter the data argument")
    if (is.null(values)) stop("Please enter the values argument")
    if (is.null(valueToConvertTo)) stop("Please enter the valueToConvertTo ",
                                        "argument")
    
    # If data isn't a data.frame, get outta here
    if (!(is.data.frame(data))) {
        stop("The data argument must be a data.frame (or data.table), or ",
             "inherit data.frame")
    }
    
    ## Get a vector of valid columns names, col_names --------------------------
    
    # Get col_names
    col_names <- names(data)
    
    # Deal with onlyConvert and noConvert
    if (!is.null(onlyConvert) & !is.null(noConvert)) {
        
        # If both onlyConvert and noConvert are specified, stop the function
        stop("Only one of the two optional arguments onlyConvert and ",
             "noConvert may be specified at the same time.")
        
    } else if (!is.null(onlyConvert)) {
        
        # onlyConvert - take any col_names out if they are in onlyConvert 
        col_names <- col_names[col_names %in% onlyConvert]
        
    } else if (!is.null(noConvert)) {
        
        # noConvert - take any col_names out if they are in noConvert
        col_names <- col_names[!col_names %in% noConvert]
    }
    
    # If col_names is empty (aka character(0)), stop the function
    if (length(col_names) == 0) {
        stop("No valid columns from data selected. Make sure that ",
             "data is a valid data.frame/data.table, and that your ",
             "onlyConvert or noConvert inputs dont results in no ",
             "columns being selected")
    }
    
    ## Do the conversion - different method based on data type
    
    if (ref == FALSE) {
        
        ## If ref is set to FALSE from its default of TRUE ---------------------
        
        # If data is a data.table, create an explicit copy of data, do the 
        # conversion by reference on that copy, then return the copy. If data
        # is not a data.table (though it should be, otherwise there is no good
        # reason to set ref to FALSE) do data.frame conversion.
        if (inherits(data, "data.table")) {
            
            # Make a copy of data
            data <- data.table::copy(data)
            
            # Remove values from col_names
            for (col_name in col_names) {
                data.table::set(data, which(data[[col_name]] %in% c(values)), 
                                col_name, valueToConvertTo) 
            } 
            
        } else {
            
            # Remove values from col_names
            data[col_names] <- lapply(data[col_names], function(f) {
                f[which(f %in% values)] <- valueToConvertTo
                return(f)
            })
        }
        
    } else if (inherits(data, "data.table")) {
        
        ## If data is a data.table ---------------------------------------------
        
        # Remove values from col_names
        for (col_name in col_names) {
            data.table::set(data, which(data[[col_name]] %in% c(values)), 
                            col_name, valueToConvertTo) 
        } 
        
        # Warn the user that modification by reference occured
        message(data_name, " modified by reference b/c it is a data.table, and",
                " ref is set to TRUE by default. Set ref to FALSE to disable ",
                "this behavior.")
        
    } else {
        
        ## If data is a data.frame --------------------------------------------- 
        
        # Remove values from col_names
        data[col_names] <- lapply(data[col_names], function(f) {
            f[which(f %in% values)] <- valueToConvertTo
            return(f)
        })
    }
    
    # Return the data
    return(data)
}