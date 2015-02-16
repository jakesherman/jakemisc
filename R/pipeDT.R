#' pipeDT()
#'
#' Evaluates "[.data.table" in a magrittr pipe-able expression.
#' 
#' @keywords data.table, evaluation, pipe, magrittr, data, table
#' @param table a data.frame or data.table.
#' @param ... arguments to be passed to "[.data.table", with the x argument
#' to "[.data.table" already being passed by the above table argument.
#' @param returnDT NULL by default, can set to TRUE or FALSE to explicitly 
#' return a data.table. By default, a data.table is returned if table is a
#' data.table, and a data.frame is returned if table is a data.frame.
#' @export
#' @examples
#' 
#' Here's an example of creating a column with dplyr, 
#' mtcars %<>%
#'    mutate(Jake = cyl + 1) %T>%
#'    pipeDT(Jake := cyl + 1)

pipeDT <- function(table, ..., returnDT = NULL) {
    
    # Load data.table
    requireNamespace(data.table)
    
    # If table isn't of the data.table class, make it one
    if (!inherits(table, "data.table")) {
        table <- data.table(table)
    }
    
    # Return the table
    if (returnDT == TRUE) {
        return(
            `[`(table, ...)
        ) 
        
    } else if (returnDT == FALSE) {
        return(as.data.frame(
            `[`(table, ...)
        )) 
        
    } else if (inherits(table, "data.table")) {
        return(
            `[`(table, ...)
        ) 
        
    } else {
        return(as.data.frame(
            `[`(table, ...)
        )) 
    }
}