#' matchup()
#'
#' Created as a quick way to match two variables that have a 1:1 
#' relationship. If more than two matches are found for an input, only the first
#' one is returned (to get around this, use merge() or a similar, more proper
#' function). 
#' 
#' Another function of mine, createMatchup(), creates closures
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
#' @param match_this column name of what you want matched (ex. state initials)...
#' @param to_this ...column name of what you want the above matched to (ex. state names)
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
    
    # Error handling
    if(is.null(require_matches)) stop(paste0("Please enter the a vector of ele",
                                             "ments that need to be matched."))
    if(is.null(matching_data)) stop(paste0("Please enter a data frame that",
                                           "contains the columns we need to",
                                           "match."))
    if(is.null(match_this)) stop("Please enter the match_this argument")
    if(is.null(to_this)) stop("Please enter the to_this argument")
    
    # Use the supplied column names to subset the supplied data frame to 
    # generate vectors
    match_this_s <- matching_data[[match_this]]
    to_this_s <- matching_data[[to_this]]
    
    # Create a function to match match_this to to_this using this sequence
    return(to_this_s[fastmatch::fmatch(require_matches, match_this_s)])
}
