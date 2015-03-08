#' changeColOrder()
#'
#' Changes the order of columns by placing a specified column to the left of 
#' another specified column.
#' 
#' This function uses seperate methods for data.tables and data.frames. By 
#' default, data.tables will be modified by reference. To turn off this
#' behavior, set \code{ref} to \code{FALSE}. 
#' 
#' @keywords change, col, column, order, column order, column ordering
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
#' \code{changeColOrder(my_data, "Jake", "Josh")}

changeColOrder <- function(data, ..., ref = TRUE, warnings = TRUE) {
    
    ## Get all of the column name changes we are doing -------------------------
    
    # Use NSE to turn ... into a character vector
    order_changes <- NSEtoVector(...)
    order_changes_list <- list()
    lorder_changes_list <- list()
    rorder_changes_list <- list()
    lorder_changes <- NULL
    rorder_changes <- NULL
    
    # If length of ... is 2 and no > or < is present, treat it as one order 
    # change (left), otherwise we have one or more order changes via > and <
    if (length(order_changes) == 2 & all(grepl(">", order_changes)) &
            all(grepl("<", order_changes))) {
        
        order_changes_list[[1]] <- c(order_changes[1], order_changes[2])
        
    } else {
        
        # Seperate into previous and future orders using > and <
        lorder_changes <- order_changes[grepl(">", order_changes)]
        rorder_changes <- order_changes[grepl("<", order_changes)]
        lorder_changes_list <- lapply(lorder_changes, seperateSymbol, ">")
        rorder_changes_list <- lapply(rorder_changes, seperateSymbol, "<")
        order_changes_list <- c(lorder_changes_list, rorder_changes_list)
        order_changes <- c(lorder_changes, rorder_changes)
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
    
    # Functions for changing column orders
    newColOrders <- function(col_names, left_of, this_col, 
                              direction = "left") {
        
        # Get column orders for the given column names, get the position of 
        # left_col and this_col within the given column names
        col_orders <- seq_along(col_names)
        left_of_pos <- match(left_of, col_names)
        this_col_pos <- match(this_col, col_names)
        
        # Get the positions to the left and right of this_col
        left_this <- col_orders[col_orders < this_col_pos]
        right_this <- col_orders[col_orders > this_col_pos]
        
        # Take left_of_pos out of left_this
        if (!is.null(left_this)) {
            if (left_of_pos %in% left_this) {
                left_this <- left_this[!left_this %in% left_of_pos]
            }
        }
        
        # Take left_of_pos out of right_this
        if (!is.null(right_this)) {
            if (left_of_pos %in% right_this) {
                right_this <- right_this[!right_this %in% left_of_pos]
            }
        }
        
        # Return the new column order
        if (direction == "left") {
            newOrder <- c(left_this, left_of_pos, this_col_pos, right_this)
            
        } else {
            newOrder <- c(left_this, this_col_pos, left_of_pos, right_this)
        }
        
        return(newOrder)
    }
    
    # Get an updated character vector of column names based on the inputted 
    # column name changes
    col_names <- names(data)
    col_order <- NULL
    
    for (i in seq_along(order_changes_list)) {
        
        ## Get the first and second columns, and if it's a left arrow ----------
        
        first_col <- order_changes_list[[i]][1]
        second_col <- order_changes_list[[i]][2]
        isLeftArrow <- grepl(">", order_changes[i])
        
        ## Error handling for each case --------------------
        
        # If current_colname isn't actually a column in data
        if (!(first_col %in% names(data)) | !(second_col %in% names(data))) {
            stop("One or more of your inputted columns are not good column ",
                 "names")
        }
        
        ## Update col_names ----------------
        if (isLeftArrow) {
            col_order <- newColOrders(col_names, first_col, second_col, "left")
            col_names <- col_names[col_order]
            
        } else {
            col_order <- newColOrders(col_names, first_col, second_col, "right")
            col_names <- col_names[col_order]
        }
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
        if ("data.table" %in% class(data)) {
            
            # Make a copy of data, change the names using setnames() to avoid
            # warnings from data.table
            data <- data.table::copy(data)
            data.table::setnames(data, names(data), col_names)
            
        } else {
            names(data) <- col_names
        }
        
    } else if ("data.table" %in% class(data)) {
        
        ## If data is a data.table ---------------------------------------------
        
        data.table::setnames(data, names(data), col_names)
        message(data_name, " modified by reference b/c it is a data.table, and",
                " ref is set to TRUE by default. Set ref to FALSE to disable ",
                "this behavior.")
        
    } else {
        
        ## If data is a data.frame --------------------------------------------- 
        
        names(data) <- col_names
    }
    
    # Return the data
    return(data)
}