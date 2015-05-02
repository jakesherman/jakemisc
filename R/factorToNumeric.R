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

#' factorToInteger()
#'
#' Turns factors into numerics. 
#' 
#' @keywords factor, numeric
#' @param a vector of factors
#' @export
#' @examples
#' 
#' factors <- factorToInteger(factors)

factorToInteger <- function(factors, intCheck = TRUE) {
    
    # Error checking - are the factors in fact whole numbers?
    if (intCheck == TRUE) {
        
        # Randomly sample up to 25 numbers
        if (length(factors >= 25)) {
            sampleSize <- 25
        } else {
            sampleSize <- length(factors)
        }
        
        sampledFactors <- sample(factors, sampleSize)
        
        # Convert the sampledFactors to numeric
        sampledFactors %<>% factorToNumeric
        
        # If any of the sampledFactors are not whole numbers, use 
        # factorToNumeric instead and throw a warning
        if (any(is.wholenumber(sampledFactors))) {
            warning("One or more of the factors you presented are not whole",
                    "numbers, and thus could not be converted into integers.",
                    " Your factors were converted into doubles instead. ",
                    "To turn off this behavior, set intCheck to FALSE.")
            return(as.numeric(as.character(factors)))
        }
    }
    
    # Perform the conversion
    return(as.integer(as.character(factors)))
}