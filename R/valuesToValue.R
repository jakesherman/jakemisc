#' valuesToValue()
#'
#' Given a data.frame or data.table, turns selected values into a different value for 
#' specified columns. By default all columns are used, but you may specify the
#' onlyConvert argument with a vector of one or more columns names and only do
#' the conversion on these columns, or alternatively, specify the noConvert
#' argument with a vector of one or more columns names that shouldnt have the
#' conversion done on them. You may only choose to specify onlyConvert or 
#' noConvert, you may not specify both at the same time. 
#' 
#' Note that this function has side effects, namely, it modifies
#' an object in its calling environment. This was necessary for data.tables, as
#' the goal is to modify them in place. I decided to do the same for data
#' frames to make the function syntax the same, even though R wont do 
#' modification in place for data frames. 
#' 
#' @keywords valuesToValue
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
#' @export
#' @examples
#' 
#' Lets say we want to convert "na" or -500 values into "jake" for only the
#' columns "town" and "city" in my_data:
#' valuesToNA(my_data, c("na", -500), "jake" ,onlyConvert = c("town", "city"))
#' 
#' Or, what if we want to convert "na" or -500 values into "jake" for every
#' columnn in my_data except for "town", "city", or "country":
#' valuesToNA(my_data, c("na", -500), "jake", noConvert = c("town", "city", 
#' "country"))

valuesToValue <- function(data = NULL, values = NULL, valueToConvertTo = NULL, 
                          onlyConvert = NULL, noConvert = NULL) {
    
    ## Error handling ----------------------------------------------------------
    
    # Missing arguments
    if (is.null(data)) stop("Please enter the data argument")
    if (is.null(values)) stop("Requires argument for values")
    if (is.null(valueToConvertTo)) stop("Requires argument for valueToConvertTo")
    
    # If data isn't a data.table or data.frame, get outta here
    if (!(is.data.frame(data) | is.data.table(data))) {
        stop("The data argument must be a data.frame/data.table")
    }
    
    # If value isn't a vector of length 1, throw an error
    if (length(valueToConvertTo) != 1 | !is.atomic(valueToConvertTo)) {
        stop("valueToConvertTo must be an atomic vector of length 1")
    }
    
    ## Get a vector of valid columns names, col_names --------------------------
    
    # Get col_names
    col_names <- names(data)
    
    # Deal with onlyConvert and noConvert
    if (!is.null(onlyConvert) & !is.null(noConvert)) {
        
        # If both onlyConvert and noConvert are specified, stop the function
        stop(paste0("Only one of the two optional arguments onlyConvert and ",
                    "noConvert may be specified at the same time."))
        
    } else if (!is.null(onlyConvert)) {
        
        # onlyConvert - take any col_names out if they are in onlyConvert 
        col_names <- col_names[col_names %in% onlyConvert]
        
    } else if (!is.null(noConvert)) {
        
        # noConvert - take any col_names out if they are in noConvert
        col_names <- col_names[!col_names %in% noConvert]
    }
    
    # If col_names is empty aka character(0), stop the function
    if (length(col_names) == 0) {
        stop(paste0("No valid columns from data selected. Make sure that ",
                    "data is a valid data.frame/data.table, and that your ",
                    "onlyConvert or noConvert inputs dont results in no ",
                    "columns being selected"))
    }
    
    # Use NSE to get the name of the data argument
    data_name <- deparse(substitute(data)) 
    
    ## Do the conversion - different method based on data type
    
    if (is.data.table(data)) {
        
        ## If data is a data.table ---------------------------------------------
        
        # Remove values from col_names
        for (col_name in col_names) {
            set(data, which(data[[col_name]] %in% c(values)), col_name, 
                valueToConvertTo)
        } 
        
    } else {
        
        ## If data is a data.frame --------------------------------------------- 
        
        # Remove values from col_names
        data[col_names] <- lapply(data[col_names], function(f) {
            f[which(f %in% c(col_names))] <- valueToConvertTo
            return(f)
        })
        
        # Modify in place (but copying the data frame) data
        command <- paste0(data_name, " <<- data")
        eval_this <- parse(text = command)
        eval(eval_this) 
    }
    
    return(invisible())
}