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

sample_up_to <- function(x, size, prob = NULL) {
    
    # Arguments: a vector (x), sample size (size), and a vector of probability
    #            weights from base::sample (prob)
    # Outputs: Sampling of of size size from x without replacement, but if the
    #          length of x is < the size argument, use length(x) as size
    
    if (length(x) < size) size <- length(x)
    sample(x, size, replace = FALSE, prob = prob)
}

# Detect if a numeric object is a whole number or not
is_wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
    abs(x - round(x)) < tol
}

## Functions for export -------------------------------------------------------

#' factor_to_numeric()
#'
#' Turns factors into numerics. 
#' 
#' @keywords factor, numeric
#' @param factors factors to convert to numeric
#' @export
#' @examples
#' 
#' factors <- factor(c(1.2, 4.2, 4.2, 3.4))
#' factors <- factor_to_numeric(factors)

factor_to_numeric <- function(factors) {
    
    # Error handling
    assert_that(is.factor(factors))
    
    # Do the conversion
    as.numeric(as.character(factors))
}

#' factor_to_integer()
#'
#' Turns factors into numerics. 
#' 
#' @keywords factor, numeric
#' @param factors a vector of factors
#' @param intCheck samples elements from factors to determine whether or not
#' they are whole numbers. If they are not whole numbers, factor_to_numeric is
#' used instead and a warning shows up. Default is \code{TRUE}, set this 
#' argument to \code{FALSE} to disable this behavior. 
#' @export
#' @examples
#' 
#' factors <- factor(c(1,2,3,4,5))
#' factors <- factor_to_integer(factors)

factor_to_integer <- function(factors = NULL, intCheck = TRUE) {
    
    # Error handling
    assert_that(is.factor(factors))
    assert_that(is.flag(intCheck))
    
    # Error handling - are the factors in fact whole numbers?
    if (intCheck) {
        
        # Randomly sample up to 50 non-NA numbers from factors and convert
        # them to numerics
        nonNAfactors <- na.omit(factors)
        sampledFactors <- sample_up_to(nonNAfactors, 50) %>%
            factor_to_numeric()
        
        # If any of the sampledFactors are not whole numbers, use 
        # factor_to_numeric instead and throw a warning
        if (any(!is_wholenumber(sampledFactors))) {
            warning("One or more of the factors you presented are not whole",
                    " numbers, and thus could not be converted into integers.",
                    " Your factors were converted into doubles instead. ",
                    "To turn off this behavior, set intCheck to FALSE.")
            newFactors <- as.numeric(as.character(factors))
            
        } else {
            newFactors <- as.integer(as.character(factors)) 
        }
        
    } else {
        newFactors <- as.integer(as.character(factors)) 
    }
    
    newFactors
}