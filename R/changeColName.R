#' changeColName()
#'
#' Changes a column name. Accepts both \code{data frame} and  \code{data.table} 
#' inputs. Multiple column names can be changed with the simple syntax 
#' \code{old_column_name/new_column_name}.
#' 
#' This function uses seperate methods for data.tables and data.frames. By 
#' default, data.tables will be modified by reference. To turn off this
#' behavior, set \code{ref} to \code{FALSE}. 
#' 
#' @keywords change, col, column, name, names
#' @param data a \code{data.frame} or \code{data.table} 
#' @param ... either a) if you are only changing one column, the current column
#' name, followed by a comma and then the desired column name (option of NSE), 
#' or b) one or more of the following: current_column_name/desired_column_name,
#' where the current column name is followed by a forward slash followed by
#' your desired column name. Using the function in this way, you may change
#' as many column names as you like in one function call.
#' @param ref TRUE (default) or FALSE, if TRUE and data is a data.table, modify 
#' the data.table by reference (modifying-in-place), if FALSE, do not modify
#' the data.table by reference, instead treat it like a data.frame (copy on
#' modify). 
#' @param warnings TRUE (default) or FALSE, should warnings occur when 
#' modifications by reference occur or conversions take place?
#' @export
#' @examples
#' 
#' Here is how to change a column name, where we change a column named "Jake"
#' into one named "Josh":
#' 
#' changeColName(my_data, "Jake", "Josh")
#' changeColName(my_data, Jake/Josh, ref = FALSE, warnings = FALSE)

changeColName <- function(data, ..., ref = TRUE, warnings = TRUE) {
    changeColName_(data, NSEtoVector(...), ref = ref, warnings = warnings)
}

#' @export
#' @rdname changeColName
changeColName_ <- function(data, ..., ref = TRUE, warnings = TRUE) {
    UseMethod("changeColName_")
}

## Methods for changeColName_ ---------------------------------------------------

#' @export
changeColName_.data.table <- function(data, ..., ref = TRUE, warnings = TRUE) {
    
    # NSE to get name of data
    data_name <- deparse(substitute(data))
    
    ## Get all of the column name changes we are doing -------------------------
    
    # Get elements of ...
    name_changes <- c(...)
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
    
    # If data isn't a data.frame, get outta here
    if (!(is.data.frame(data))) {
        stop("The data argument must be a data.frame (or data.table), or ",
             "inherit data.frame")
    }
    
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
                    "like to use a different desired_colname? [y/n]")
                command <- scan(what = character(), n = 1, quiet = TRUE) 
                
                # Modify the install variable based on user input
                if (command == "y") {
                    
                    cat("Please enter your new desired_colname, or press enter",
                        " to stop the function ['name'/ENTER to end]")
                    new_name <- scan(what = character(), quiet = TRUE)
                    
                    if (length(new_name) == 0) {
                        stop()
                    } else {
                        desired_colname <- new_name
                    }
                    
                } else if (command == "n") {
                    # Do nothing
                } else {
                    stop("You inputted something other than [y/n] in the prompt")
                }
                
            } else if (warnings) {
                warning("desired_colname is already a column name")
            }
        }
        
        # Update col_names 
        col_names <- newColNames(col_names, current_colname, desired_colname)
    }
    
    ## Do the column name changing ---------------------------------------------
    
    if (ref == FALSE) {
        
        # Make a copy of data, change the names using setnames() to avoid
        # warnings from data.table
        
        data <- data.table::copy(data)
        setnames(data, names(data), col_names)
        return(data)
        
    } else {
        
        data.table::setnames(data, names(data), col_names)
        message(data_name, " modified by reference b/c it is a data.table, and",
                " ref is set to TRUE by default. Set ref to FALSE to disable ",
                "this behavior.")
        
        return(invisible(data))
    } 
}

#' @export
changeColName_.data.frame <- function(data, ..., ref = TRUE, warnings = TRUE) {
    
    # NSE to get name of data
    data_name <- deparse(substitute(data))
    
    ## Get all of the column name changes we are doing -------------------------
    
    # Get elements of ...
    name_changes <- c(...)
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
                    "like to use a different desired_colname? [y/n]")
                command <- scan(what = character(), n = 1, quiet = TRUE) 
                
                # Modify the install variable based on user input
                if (command == "y") {
                    
                    cat("Please enter your new desired_colname, or press enter",
                        " to stop the function ['name'/ENTER to end]")
                    new_name <- scan(what = character(), quiet = TRUE)
                    
                    if (length(new_name) == 0) {
                        stop()
                    } else {
                        desired_colname <- new_name
                    }
                    
                } else if (command == "n") {
                    # Do nothing
                } else {
                    stop("You inputted something other than [y/n] in the prompt")
                }
                
            } else if (warnings) {
                warning("desired_colname is already a column name")
            }
        }
        
        # Update col_names 
        col_names <- newColNames(col_names, current_colname, desired_colname)
    }
    
    ## Do the column name changing ----------------------------------------------
    
    names(data) <- col_names
    return(data) 
}