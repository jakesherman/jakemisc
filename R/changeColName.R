#' changeColName()
#'
#' Changes a column name from one to another. Accepts both data frames and 
#' data.tables. 
#' 
#' Note that this function has side effects, namely, it modifies
#' an object in its calling environment. This was necessary for data.tables, as
#' the goal is to modify them in place. I decided to do the same for data
#' frames to make the function syntax the same, even though R won't do 
#' modification in place for data frames. 
#' 
#' @keywords matchup, match, up, lookup, crosswalk
#' @param data a data.frame or data.table 
#' @param ... either a) if you are only changing one column, the current column
#' name, followed by a comma and then the desired column name (option of NSE), 
#' or b) one or more of the following: current_column_name/desired_column_name,
#' where the current column name is followed by a forward slash followed by
#' your desired column name. Using the function in this way, you may change
#' as many column names as you like in one function call.
#' @param ref TRUE (default) or FALSE, if TRUE and data is a data.table, modify 
#' the data.table by reference (modifying-in-place), if FALSE, do not modify
#' the data.table by reference, instead treat it like a data.frame (copy on
#' modify). When combining this function with the magrittr package, use the
#' \code{\%T>\%} operator before this function to modify-in-place.
#' @param warnings TRUE (default) or FALSE, should warnings occur when 
#' modifications by reference occur or conversions take place?
#' @export
#' @examples
#' 
#' Here is how to change a column name, where we change a column named "Jake"
#' into one named "Josh":
#' 
#' changeColName(my_data, "Jake", "Josh")

changeColName <- function(data, ..., ref = TRUE, warnings = TRUE) {
    
    ## Get all of the column name changes we are doing -------------------------
    
    # Use NSE to turn ... into a character vector
    name_changes <- NSEtoVector(...)
    name_changes_list <- list()
    
    # If length of ... is 2 and no / is present, treat it as one name change, 
    # otherwise we have one or more name changes that use forward slashes
    if (length(name_changes) == 2 & !all(grepl("/", name_changes))) {
        
        name_changes_list[[1]] <- c(name_changes[1], name_changes[2])
        
    } else {
        
        # Seperate into previous and future colname using /
        name_changes_list <- lapply(name_changes, seperateSymbol, "/")
    }
    
    ## Error handling ----------------------------------------------------------
    
    # If arguments are missing
    if (is.null(data)) stop("Requires argument for data")
    
    ## Get a vector of new column names ----------------------------------------
    
    # Get new column names
    newColNames <- function(col_names, current_colname, desired_colname) {
        col_names[col_names == current_colname] <- desired_colname
        return(col_names)
    }
    
    # Get an updated character vector of column names based on the inputted 
    # column name changes
    col_names <- names(data)
    for (i in seq_along(name_changes_list)) {
        
        ## Get the current and desired column names ---------------
        
        current_colname <- name_changes_list[[i]][1]
        desired_colname <- name_changes_list[[i]][2]
        
        ## Error handling for each case --------------------
        
        # If current_colname isn't actually a column in data
        if (!(current_colname %in% names(data))) {
            stop("Your current_colname argument doesn't match any column ",
                 "names of your data argument. Please check your spelling.")
        }
        
        # If desired_colname is already a column name
        if (desired_colname %in% names(data)) {
            if (warnings & interactive()) {
                
                cat("Your desired_colname is already a column name. Would you ",
                    "still like to change your column name? [y/n]")
                command <- scan(what = character(), n = 1, quiet = TRUE) 
                
                # Modify the install variable based on user input
                if (command == "y") {
                    stop()
                } else if (command == "n") {
                    # Do nothing
                } else {
                    stop("You inputted something other than [y/n] in the prompt")
                }
                
            } else if (warnings) {
                warning("desired_colname is already a column name")
            }
        }
        
        ## Update col_names ----------------
        col_names <- newColNames(col_names, current_colname, desired_colname)
    }
    
    ## Do the column name changing ---------------------------------------------
    
    # Change the column names(varies if we have a data.table or
    # a data.frame)]
    if (ref == FALSE) {
        
        ## If ref is set to FALSE from its default of TRUE ---------------------
        
        # If data is a data.table, create an explicit copy of data, do the 
        # conversion by reference on that copy, then return the copy. If data
        # is not a data.table (though it should be, otherwise there is no good
        # reason to set ref to FALSE) do data.frame conversion.
        if (inherits(data, "data.table") & isPackageInstalled("data.table")) {
            
            # Make a copy of data, change the names using setnames() to avoid
            # warnings from data.table
            data <- copy(data)
            setnames(data, names(data), col_names)
            return(data)
            
        } else {
            
            names(data) <- col_names
            return(data)
        }
        
    } else if ("data.table" %in% class(data) & 
                   isPackageInstalled("data.table")) {
        
        ## If data is a data.table ---------------------------------------------
        
        setnames(data, names(data), col_names)
        return(invisible())
        
    } else {
        
        ## If data is a data.frame --------------------------------------------- 
        
        names(data) <- col_names
        return(data)
    }
}