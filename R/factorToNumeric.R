#' factorToNumeric()
#'
#' Turns factors into numerics. 
#' 
#' @keywords factor, numeric
#' @param factors a vector of factors
#' @export
#' @examples
#' 
#' factors <- factor(c(1.2, 4.2, 4.2, 3.4))
#' factors <- factorToNumeric(factors)

factorToNumeric <- function(factors) {
    
    # Error checking - is the input a factor? Is its length >= 1?
    assertthat::assert_that(is.factor(factors))
    assertthat::assert_that(length(factors) >= 1)
    
    # Do the conversion
    as.numeric(as.character(factors))
}

#' factorToInteger()
#'
#' Turns factors into numerics. 
#' 
#' @keywords factor, numeric
#' @param factors a vector of factors
#' @param intCheck samples elements from factors to determine whether or not
#' they are whole numbers. If they are not whole numbers, factorToNumeric is
#' used instead and a warning shows up. Default is \code{TRUE}, set this 
#' argument to \code{FALSE} to disable this behavior. 
#' @export
#' @examples
#' 
#' factors <- factor(c(1,2,3,4,5))
#' factors <- factorToInteger(factors)

factorToInteger <- function(factors, intCheck = TRUE) {
    
    # Error checking - is the input a factor? Is its length >= 1?
    assertthat::assert_that(is.factor(factors))
    assertthat::assert_that(length(factors) >= 1)
    
    # Error checking - are the factors in fact whole numbers?
    if (intCheck == TRUE) {
        
        # Randomly sample up to 50 numbers
        if (length(factors) >= 50) {
            sampleSize <- 50
        } else {
            sampleSize <- length(factors)
        }
        
        sampledFactors <- sample(factors, sampleSize)
        
        # Convert the sampledFactors to numeric
        sampledFactors %<>% factorToNumeric()
        
        # If any of the sampledFactors are not whole numbers, use 
        # factorToNumeric instead and throw a warning
        if (any(!is.wholenumber(sampledFactors))) {
            warning("One or more of the factors you presented are not whole",
                    " numbers, and thus could not be converted into integers.",
                    " Your factors were converted into doubles instead. ",
                    "To turn off this behavior, set intCheck to FALSE.")
            newFactors <- as.numeric(as.character(factors))
            
        } else {
            newFactors <- as.integer(as.character(factors))
        }
    }
    
    newFactors
}