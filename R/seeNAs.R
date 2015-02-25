#' changeColName()
#'
#' See the number or percent of rows that are NAs in a data.frame, or see the
#' number or perf
#' 
#' @keywords matchup, match, up, lookup, crosswalk
#' @param data a \code{data.frame} or \code{data.table} 
#' @param type
#' @export
#' @examples
#' 
#' Here is how to change a column name, where we change a column named "Jake"
#' into one named "Josh":
#' 
#' \code{changeColName(my_data, "Jake", "Josh")}

seeNAs <- function(data, type = "NAs") {
    
    if (type == "NAs") {
        return(sapply(data, function(f) sum(is.na(f))))
        
    } else if (type == "")
    
}