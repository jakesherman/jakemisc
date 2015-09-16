## ============================================================================
##
## Assertations for functions in jakemisc
## Created via the assertthat package
##
## ============================================================================

## notNULL --------------------------------------------------------------------

# Returns TRUE if an object is not NULL

notNULL <- function(x) !is.null(x)

assertthat::on_failure(notNULL) <- function(call, env) {
    paste0("Please fill in the '", deparse(call$x), "' argument.")
}

## is.DT ----------------------------------------------------------------------

# Is an object a data.frame? Same function as data.table::is.data.table, but
# I am copying it here to add the 'fail' attribute

is.DT <- function(x) inherits(x, "data.table")

assertthat::on_failure(is.DT) <- function(call, env) {
    paste0(deparse(call$x), " is not a data.table.")
}

## namesIn --------------------------------------------------------------------

# Are all of the elements in the character vector names in names(df)?

namesIn <- function(names, df) {
    
    # Arguments: character vector (names), a data.frame (df) w/ names
    # Outputs: are all of names in names(df)? TRUE or FALSE.
    
    assertthat::assert_that(is.character(names))
    all(names %in% names(df))
}

assertthat::on_failure(namesIn) <- function(call, env = parent.env) {
    paste0("One or more of the names in '", deparse(call$x), "' is not a name",
           " in the data.frame")
}

## exists ---------------------------------------------------------------------

exists <- function(...) base::exists(...)

assertthat::on_failure(exists) <- function(call, env = parent.env) {
    paste0("Object does not exist.")
}

## notIdentical ---------------------------------------------------------------

notIdentical <- function(x, y) !identical(x, y)

assertthat::on_failure(notIdentical) <- function(call, env = parent.env) {
    paste0("The objects you have provided are identical.")
}

## isVector -------------------------------------------------------------------

isVector <- function(x) {
    
    # https://stackoverflow.com/questions/19501186/how-to-test-if-object-
    # is-a-vector
    mode(x) %in% c("logical", "numeric", "complex", "character")
}

assertthat::on_failure(isVector) <- function(call, env) {
    paste0("Argument '", deparse(call$x), "' is not a vector.")
}

## isVector -------------------------------------------------------------------

is_vector <- function(x) {
    
    # https://stackoverflow.com/questions/19501186/how-to-test-if-object-
    # is-a-vector
    mode(x) %in% c("logical", "numeric", "complex", "character")
}

assertthat::on_failure(is_vector) <- function(call, env) {
    paste0("Argument '", deparse(call$x), "' is not a vector.")
}

## no_duplicates ----------------------------------------------------------------

no_duplicates <- function(x) sum(duplicated(x)) == 0

assertthat::on_failure(no_duplicates) <- function(call, env)
    paste0("Duplicates exist in ", deparse(call$x), " when they should be none.")

