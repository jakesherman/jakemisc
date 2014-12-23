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
#' @param current_colname the column name that you want to change
#' @param desired_colname the column name that you current_colname changed to
#' @export
#' @examples
#' 
#' Here is how to change a column name, where we change a column named "Jake"
#' into one named "Josh":
#' 
#' changeColName(my_data, "Jake", "Josh")

changeColName <- function(data, current_colname = NULL, desired_colname = NULL,
                          ref = NULL, warnings = NULL) {
    
    ## Error handling
    
    # If arguments are missing
    if (is.null(desired_colname)) stop("Requires argument for desired_colname")
    if (is.null(current_colname)) stop("Requires argument for current_colname")
    if (is.null(data)) stop("Requires argument for data")
    
    # If current_colname isn't actually a column in data
    if (!(current_colname %in% names(data))) {
        stop("Your current_colname argument doesn't match any column ",
             "names of your data argument. Please check your spelling.")
    }
    
    # If desired_colname is already a column name
    if (desired_colname %in% names(data)) {
        if (warnings & interactive()) {
            
            cat("Your desired_colname is already a column name. Would you stil",
                "l like to change your column name? [y/n]")
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
    
    # Get new column names
    col_names <- names(data)
    col_names[col_names == current_colname] <- desired_colname
    
    # Change the column names(varies if we have a data.table or
    # a data.frame)]
    if (ref == FALSE) {
        
        ## If ref is set to FALSE from its default of TRUE ---------------------
        
        # If data is a data.table, create an explicit copy of data, do the 
        # conversion by reference on that copy, then return the copy. If data
        # is not a data.table (though it should be, otherwise there is no good
        # reason to set ref to FALSE) do data.frame conversion.
        if ("data.table" %in% class(data) & isPackageInstalled("data.table")) {
            
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