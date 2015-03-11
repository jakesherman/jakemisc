## =============================================================================
##
## Various matchup functions - contains:
##     1) matchup()
##     2) createMatchup()
##
## =============================================================================

#' matchup()
#'
#' Created as a quick way to match two variables that have a 1:1 
#' relationship. If more than two matches are found for an input, only the first
#' one is returned (to get around this, use merge() or a similar, more proper
#' function). 
#' 
#' Another function of mine, \code{createMatchup()}, creates closures
#' based on this function, which can be useful for creating reuseable matching
#' functions on the fly. 
#' 
#' This function works by taking a data frame ("matching data") and the column
#' names of 
#' 
#' @keywords matchup, match, up, lookup, crosswalk, 1:1
#' @param require_matches a character vector of elements that need to be matched
#' @param matching_data a data frame containing columns matching two things,
#' i.e. the column names you are inputting in the match_this and to_this 
#' arguments.
#' @param match_this column name of what you want matched (ex. state 
#' initials)...
#' @param to_this ...column name of what you want the above matched to (ex. 
#' state names)
#' @export
#' @examples
#' 
#' Imagine that we have a vector of state initials ("MA", "CA", "NY"), and we
#' want to get state names for those initials. Because there is a 1:1 
#' relationship between state initials and names, matchup() will do the trick.
#' Supply the matchup() function 1) your vector of state initials, 2) a data
#' frame containing a column of state initials and a matching column of state
#' names, 3) the column name of state initials, and 4) the column name of state
#' names. Below is the code to do so:
#' 
#' matchup(vector_of_state_initials, us_states, "state", "state_name")
#'                        
#' And the result is a vector: ("Massachusetts", "Califorina", "New York")

matchup <- function(require_matches, matching_data, match_this, to_this) {
    matchup_(require_matches, matching_data, NSEtoVector(match_this),
             NSEtoVector(to_this))
}

#' @export
#' @rdname matchup
matchup_ <- function(require_matches, matching_data, match_this, to_this) {
    
    # Error handling
    if (is.null(require_matches)) stop("Please enter the a vector of ele ",
                                       "ments that need to be matched.")
    if (is.null(matching_data)) stop("Please enter a data frame that ",
                                     "contains the columns we need to ",
                                     "match.")
    if (is.null(match_this)) stop("Please enter the match_this argument")
    if (is.null(to_this)) stop("Please enter the to_this argument")
    
    # Use the supplied column names to subset the supplied data frame to 
    # generate vectors
    match_this_s <- matching_data[[match_this]]
    to_this_s <- matching_data[[to_this]]
    
    # Create a function to match match_this to to_this using this sequence
    return(to_this_s[fastmatch::fmatch(require_matches, match_this_s)])
}

#' createMatchup()
#'
#' Creates a closure of the \code{matchup()} function that stores with the 
#' \code{matchup()} function data for all of its arguments except for its first, 
#' the vector of elements that need to be matched. This allows one to create 
#' quick, 1:1 matching functions that can be reused, akin to Excel's VLOOKUP
#' or INDEX/MATCH functionality.
#' 
#' @keywords matchup, match, up, lookup, crosswalk
#' @param matching_data a data frame containing columns matching two things,
#' i.e. the column names you are inputting in the match_this and to_this 
#' arguments.
#' @param match_this column name of what you want matched (ex. state 
#' initials)...
#' @param to_this ...column name of what you want the above matched to (ex. 
#' state names)
#' @export
#' @examples
#' 
#' Let's try using createMatchup to create a function that matches US state
#' initials to state names, like the example for the matchup() function. 
#' Remember, this is how we matched state initials to names before:
#' 
#' \code{matchup(vector_of_state_initials, us_states, "state", "state_name")}
#' 
#' Now let's create a function to do this, called initials_to_names():
#' 
#' \code{initals_to_names <- createMatchup(us_states, "state", "state_name")}
#' 
#' Now, we just need to input the vector_of_state_initials that was the first
#' argument in the matchup() function above to produce its output when it comes
#' to matching state names to initials. This:
#' 
#' \code{initials_to_names(vector_of_state_initials)}
#' 
#' Produces the same output as this:
#' 
#' \code{matchup(vector_of_state_initials, us_states, "state", "state_name")}

createMatchup <- function(matching_data, match_this, to_this) {
    
    # Error handling (parent)
    if (is.null(matching_data)) stop("Please enter a data frame that ",
                                    "contains the columns we need to ",
                                    "match.")
    if (is.null(match_this)) stop("Please enter the match_this argument")
    if (is.null(to_this)) stop("Please enter the to_this argument")
    
    function(require_matches) {
        
        # Error handling (child)
        if(is.null(require_matches)) stop("Please enter the a vector of",
                                          " elements that need to be",
                                          " matched.")
        
        # Use the supplied column names to subset the supplied data frame to 
        # generate vectors
        match_this_s <- matching_data[[match_this]]
        to_this_s <- matching_data[[to_this]]
        
        # Create a function to match match_this to to_this using this sequence
        return(to_this_s[fastmatch::fmatch(require_matches, match_this_s)])
    }
}