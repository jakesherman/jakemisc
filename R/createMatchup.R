#' createMatchup()
#'
#' Creates a closure of the matchup() function that stores with the matchup()
#' function data for all of its arguments except for its first, the vector of 
#' elements that need to be matched. This allows one to create quick, 1:1 
#' matching functions that can be reused.
#' 
#' @keywords matchup, match, up, lookup, crosswalk
#' @param matching_data a data frame containing columns matching two things,
#' i.e. the column names you are inputting in the match_this and to_this 
#' arguments.
#' @param match_this column name of what you want matched (ex. state initials)...
#' @param to_this ...column name of what you want the above matched to (ex. state names)
#' @export
#' @examples
#' 
#' Let's try using createMatchup to create a function that matches US state
#' initials to state names, like the example for the matchup() function. 
#' Remember, this is how we matched state initials to names before:
#' 
#' matchup(vector_of_state_initials, us_states, "state", "state_name")
#' 
#' Now let's create a function to do this, called initials_to_names():
#' 
#' initals_to_names <- createMatchup(us_states, "state", "state_name")
#' 
#' Now, we just need to input the vector_of_state_initials that was the first
#' argument in the matchup() function above to produce its output when it comes
#' to matching state names to initials. This:
#' 
#' initials_to_names(vector_of_state_initials)
#' 
#' Produces the same output as this:
#' 
#' matchup(vector_of_state_initials, us_states, 
#'                        "state", "state_name")

createMatchup <- function(matching_data, match_this, to_this) {
    
    # Error handling (parent)
    if(is.null(matching_data)) stop(paste0("Please enter a data frame that",
                                           "contains the columns we need to",
                                           "match."))
    if(is.null(match_this)) stop("Please enter the match_this argument")
    if(is.null(to_this)) stop("Please enter the to_this argument")
    
    function(require_matches) {
        
        # Error handling (child)
        if(is.null(require_matches)) stop(paste0("Please enter the a vector of",
                                                 " elements that need to be",
                                                 " matched."))
        
        # Use the supplied column names to subset the supplied data frame to 
        # generate vectors
        match_this_s <- matching_data[[match_this]]
        to_this_s <- matching_data[[to_this]]
        
        # Create a function to match match_this to to_this using this sequence
        return(to_this_s[fastmatch::fmatch(require_matches, match_this_s)])
    }
}
