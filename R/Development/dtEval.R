#' dtEval()
#'
#' Evaluates [.data.table in a magrittr pipe-able expression.
#' 
#' @keywords data.table, evaluation, pipe, magrittr, data, table
#' @param table a data.frame or data.table.
#' @param ... arguments to be passed to [.data.table, with the x argument
#' to [.data.table already being passed by the above table argument.
#' @param returnDT NULL by default, can set to TRUE or FALSE to explicitly 
#' return a data.table. By default, a data.table is returned if table is a
#' data.table, and a data.frame is returned if table is a data.frame.
#' @param invisible TRUE (by default), invisibly return the data?
#' @export
#' @examples
#' 
#' Heres an example of creating a column with dplyr, 
#' mtcars %<>%
#'    mutate(Jake = cyl + 1) %T>%
#'    dtEval(Jake := cyl + 1)

dtEval <- function(table, ..., returnDT = NULL, invisible = TRUE) {
    
    # If table isn't of the data.table class, make it one
    if (!inherits(table, "data.table")) {
        table <- data.table(table)
    }
    
    # Set returnDT to 0 if NULL
    if (is.null(returnDT)) returnDT <- 0
    
    # Return the data
    if (invisible == TRUE) {
        
        # Return the table invisibly
        if (returnDT == TRUE) {
            return(invisible(
                `[`(table, ...)
            ))
            
        } else if (returnDT == FALSE) {
            return(as.data.frame(invisible(
                `[`(table, ...)
            )))
            
        } else if (inherits(table, "data.table")) {
            return(invisible(
                `[`(table, ...)
            ))
            
        } else {
            return(as.data.frame(invisible(
                `[`(table, ...)
            )))
        }
        
    } else {
        
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
}