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

changeColName <- function(data, current_colname, desired_colname) {
    
    ## Error handling
    
    # If arguments are missing
    if (is.null(desired_colname)) stop("Requires argument for desired_colname")
    if (is.null(current_colname)) stop("Requires argument for current_colname")
    if (is.null(data)) stop("Requires argument for data")
    
    # If current_colname isn't actually a column in data
    if (!(current_colname %in% names(data))) {
        stop(paste0("Your current_colname argument doesn't match any column ",
                    "names of your data argument. Please check your spelling."))
    }
    
    # If desired_colname is already a column name
    if (desired_colname %in% names(data)) {
        stop("desired_colname is already a column name")
    }
    
    # Get new column names
    col_names <- names(data)
    col_names[col_names == current_colname] <- desired_colname
    
    # Change the column names(varies if we have a data.table or
    # a data.frame)
    if (is.data.table(data)) {
        setnames(data, names(data), col_names)
    } else {
        data_name <- deparse(substitute(data))
        command <- paste0("names(", data_name, ") <<- col_names")
        eval_this <- parse(text = command)
        eval(eval_this)
    }
    
    return(invisible())
}