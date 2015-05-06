## ============================================================================
##
## Functions to coerse factors
##
## Background: base::as.numeric() and base::as.integer() operate on the levels
## of factors, not the values themselves, which typically isn't what one
## wants. Since the above functions are primitives, I thought it would be
## best to just write new functions to do coersion from factors to numerics.
##
## ============================================================================

## Functions not for export ---------------------------------------------------

sampleUpTo <- function(x, size, prob = NULL) {
    
    # Arguments: a vector (x), sample size (size), and a vector of probability
    #            weights from base::sample (prob)
    # Outputs: Sampling of of size size from x without replacement, but if the
    #          length of x is < the size argument, use length(x) as size
    
    if (length(x) < size) size <- length(x)
    sample(x, size, replace = FALSE, prob = prob)
}

## Functions for export -------------------------------------------------------

#' factorToNumeric()
#'
#' Turns factors into numerics. 
#' 
#' @keywords factor, numeric
#' @param factors factors to convert to numeric
#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @importFrom magrittr "%T>%"
#' @importFrom magrittr "%$%"
#' @export
#' @examples
#' 
#' factors <- factor(c(1.2, 4.2, 4.2, 3.4))
#' factors <- factorToNumeric(factors)

factorToNumeric <- function(factors) {
    
    # Error handling
    assertthat::assert_that(notNULL(factors))
    assertthat::assert_that(is.factor(factors))
    
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

factorToInteger <- function(factors = NULL, intCheck = TRUE) {
    
    # Error handling
    assertthat::assert_that(notNULL(factors))
    assertthat::assert_that(is.factor(factors))
    assertthat::assert_that(assertthat::is.flag(intCheck))
    
    # Error handling - are the factors in fact whole numbers?
    if (intCheck) {
        
        # Randomly sample up to 50 non-NA numbers from factors and convert
        # them to numerics
        nonNAfactors <- na.omit(factors)
        sampledFactors <- sampleUpTo(nonNAfactors, 50) %>%
            factorToNumeric()
        
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