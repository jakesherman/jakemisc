#' factorToNumeric()
#'
#' Turns factors into numerics. 
#' 
#' @keywords factor, numeric
#' @param a vector of factors
#' @export
#' @examples
#' 
#' factors <- factorToNumeric(factors)

factorToNumeric <- function(factors) {
    return(as.numeric(as.character(factors)))
}