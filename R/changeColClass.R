#' changeColClass()
#'
#' Changes the class of columns from one class to another class, ex. change all
#' columns of class factor into columns of class character. Works on both
#' \code{data.frame} and \code{data.table} classes. You may also specify either 
#' a) specific columns to do this conversion on, or alternatively b) specific 
#' columns that you do not want conversion performed on. 
#' 
#' This function uses seperate methods for data.tables and data.frames. By 
#' default, data.tables will be modified by reference. To turn off this
#' behavior, set \code{ref} to \code{FALSE}. 
#' 
#' @keywords convert, data.table, factor, numeric, character, data, table, frame,
#' data.frame, class
#' @param data a \code{data.frame} or \code{data.table} 
#' @param startingClass the column type you want converted
#' @param finalClass the column type you want to convert startingClass into
#' @param onlyConvert OPTIONAL - specify a vector of one or more column names
#' as the only columns where the class conversion will take place. You may 
#' either have this parameter satified, or the noConvert parameter specified, 
#' you may not specify both.
#' @param noConvert OPTIONAL - specify a vector of one or more columns names 
#' that you do not want any conversion applied to. You may either have this 
#' parameter satified, or the noConvert parameter specified, you may not 
#' specify both.
#' @param ref TRUE (default) or FALSE, if TRUE and data is a data.table, modify 
#' the data.table by reference (modifying-in-place), if FALSE, do not modify
#' the data.table by reference, instead treat it like a data.frame (copy on
#' modify). 
#' @export
#' @examples
#' 
#' \code{changeColClass(my_data, "factor", "character")}
#' \code{changeColClass(my_data, "numeric", "character", 
#'                      onlyConvert = c(my_cols))}
#' \code{changeColClass(my_data, "numeric", "character", 
#'                      noConvert = "special_col")}

changeColClass <- function(data = NULL, startingClass = NULL, finalClass = NULL,
                           onlyConvert = NULL, noConvert = NULL, ref = TRUE) {
    
    # NSE to get name of data
    data_name <- deparse(substitute(data))
    
    ## Error handling ----------------------------------------------------------
    
    # Missing arguments
    if (is.null(data)) stop("Requires the data argument")
    if (is.null(startingClass)) stop("Requires argument for startingClass")
    if (is.null(finalClass)) stop("Requires argument for finalClass")
    
    # If data isn't a data.frame, get outta here
    if (!(is.data.frame(data))) {
        stop("The data argument must be a data.frame (or data.table), or ",
             "inherit data.frame")
    }
    
    ## Get the column names of the columns that are of the class entered by
    ## the startingClass argument, and use NSE to get the name of data ---------
    
    # Get col_names
    sapply_exp <- paste0("sapply(data, is.", startingClass, ")")
    eval_this <- parse(text = sapply_exp)
    col_names <- names(data)[eval(eval_this)]
    
    # Deal with onlyConvert and noConvert
    if (!is.null(onlyConvert) & !is.null(noConvert)) {
        
        # If both onlyConvert and noConvert are specified, stop the function
        stop("Only one of the two optional arguments onlyConvert and ",
                    "noConvert may be specified at the same time.")
        
    } else if (!is.null(onlyConvert)) {
        
        # onlyConvert - take any col_names out if they aren't in onlyConvert 
        col_names <- col_names[col_names %in% onlyConvert]
        
    } else if (!is.null(noConvert)) {
        
        # noConvert - take any col_names out if they are in noConvert
        col_names <- col_names[!col_names %in% noConvert]
    }
    
    # If col_names is empty aka character(0), stop the function, there are
    # no columns of type startingClass in your data
    if (length(col_names) == 0) {
        stop("There were no columns found in the data of the ",
             "type you entered in startingClass. Please check ",
             "the above output of the class of each column ",
             "in your data to see if the class you entered ",
             "in startingClass exists.")
    }
    
    ## Do the conversion - different method based on data type. Turn this into
    ## S3 at some point? Or not worth it?
    
    if (ref == FALSE) {
        
        ## If ref is set to FALSE from its default of TRUE ---------------------
        
        # If data is a data.table, create an explicit copy of data, do the 
        # conversion by reference on that copy, then return the copy. If data
        # is not a data.table (though it should be, otherwise there is no good
        # reason to set ref to FALSE) do data.frame conversion.
        
        if (inherits(data, "data.table")) {
            
            # Make an explicit copy of the data
            data <- data.table::copy(data)
            
            # Data.table conversion
            if (startingClass == "factor" & finalClass == "numeric") {
                data[, (col_names) := lapply(.SD, factorToNumeric), 
                     .SDcols = col_names]
                
            } else {
                lapply_exp <- paste0("lapply(.SD, as.", finalClass, ")")
                eval_this <- parse(text = lapply_exp)
                data[, (col_names) := eval(eval_this), .SDcols = col_names]
            }
            
        } else {
            
            # Data.frame conversion
            
            if (startingClass == "factor" & finalClass == "numeric") {
                
                # Special case where we are going from factor to numeric. 
                data[col_names] <- lapply(data[col_names], factorToNumeric)
                
            } else {
                
                # Convert col_names to the appropirate column type
                lapply_exp <- paste0("data[col_names] <- lapply(data[col_names", 
                                     "], as.", finalClass, ")")
                eval_this <- parse(text = lapply_exp)
                eval(eval_this)
            }
        }
        
    } else if (inherits(data, "data.table")) {
        
        ## Modifying a data.table by reference ---------------------------------
        
        if (startingClass == "factor" & finalClass == "numeric") {
            
            # Special case where we are going from factor to numeric. 
            data[, (col_names) := lapply(.SD, factorToNumeric), 
                 .SDcols = col_names]
            
        } else {
            
            # Convert col_names to the appropirate column type
            lapply_exp <- paste0("lapply(.SD, as.", finalClass, ")")
            eval_this <- parse(text = lapply_exp)
            data[, (col_names) := eval(eval_this), .SDcols = col_names]
        }
        
        # Message that data was modified by reference
        message(data_name, " modified by reference b/c it is a data.table, and",
                "ref is set to TRUE by default. Set ref to FALSE to disable ",
                "this behavior.")
        
    } else {
        
        ## Dealing with a data.frame -------------------------------------------
        
        if (startingClass == "factor" & finalClass == "numeric") {
            
            # Special case where we are going from factor to numeric. 
            data[col_names] <- lapply(data[col_names], factorToNumeric)
            
        } else {
            
            # Convert col_names to the appropirate column type
            lapply_exp <- paste0("data[col_names] <- lapply(data[col_names], ", 
                                 "as.", finalClass, ")")
            eval_this <- parse(text = lapply_exp)
            eval(eval_this)
        }
    }
    
    # Return data
    return(data)
}