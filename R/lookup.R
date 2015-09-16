## ============================================================================
##
## Functions related to lookup objects and tables
##
## ============================================================================

#' lookup()
#' 
#' Creates a lookup object. Lookup objects contain two equal length vectors:
#' a key, and a value, where elements in key map to elements in value. Lookup
#' objects function similar to named vectors, but are optimized such that 
#' subsetting on them is very quick.
#'
#' @keywords lookup, key, value, fast, match, conversion
#' @param key a vector of unique keys
#' @param value a vector of values corresponding to the keys. These values
#' do not need to be unique, but the two vectors (key and value) must be of
#' the same length
#' @export
#' @examples 
#' 
#' letter_lookup <- lookup(LETTERS, 1:26)
#' match_these <- sample(LETTERS, 10000000, TRUE)
#' letter_lookup[match_these]

lookup <- function(keys, values, fix_duplicates = TRUE, strict = TRUE) {
    
    # Error handling
    assert_that(is_vector(keys))
    assert_that(is_vector(values))
    assert_that(equal_length(keys, values))
    
    # Take care of duplicates - if fix_duplicates is TRUE, allow duplicate
    # keys as inputs and fix them after the fact. If FALSE, keys need to be
    # deduplicated before being fed as inputs to the function
    if (fix_duplicates) {
        
        # Get the uniquw keys
        unique_keys <- unique(keys)
        
        # If keys isn't totally unique, do the following:
        if (!equal_length(unique_keys, keys)) {
            if (strict) {
                
                # By being strict, we are testing to make sure that all keys
                # with multiple values contain the SAME values. If we don't 
                # do this, the first value out of multiple values for a key 
                # will be assigned as the value to that key
                
                if (all(vapply(split(values, keys), {
                    function(f) length(unique(f)) == 1
                }, logical(1)))) {
                    
                    # Grab the values that correspond to the unique keys
                    values <- values[fastmatch::fmatch(unique_keys, keys)]
                    
                } else {
                    
                    # One or more values aren't distinct for their keys
                    stop("One or more of your keys are associated with ",
                         "different values, stopped creating the lookup. ",
                         "Set strict to FALSE to turn off this behavior.")
                }
                
            } else {
                
                # Grab the values that correspond to the unique keys
                values <- values[fastmatch::fmatch(unique_keys, keys)]
            }
        }
        
        keys <- unique_keys
        
    } else {
        assert_that(no_duplicates(keys))
    }
    
    # Create the lookup object
    lookup <- named_list(keys, values)
    structure(lookup, class = "lookup")
}

#' lookup_function()
#' 
#' A closure that returns a function that takes in values to match on, and
#' returns matches for those values.
#'
#' @keywords lookup, function, key, value, closure
#' @param key a vector of unique keys
#' @param value a vector of values corresponding to the keys. These values
#' do not need to be unique, but the two vectors (key and value) must be of
#' the same length
#' @export
#' @examples 
#' 
#' letter_convert <- lookup_function(LETTERS, 1:26)
#' letter_convert(c("A", "B", "D"))

lookup_function <- function(key, value) 
    function(match_these) lookup(key, value)[match_these]

#' perform_lookup()
#' 
#' A function that performs conversions to values based on keys.
#'
#' @keywords perform, lookup, key, value
#' @param match_these a vector that is matched against \code{key} to return 
#' \code{value}
#' @param key a vector of unique keys
#' @param value a vector of values corresponding to the keys. These values
#' do not need to be unique, but the two vectors (key and value) must be of
#' the same length
#' @export
#' @examples 
#' 
#' perform_lookup(c("A", "B", "D"), LETTERS, 1:26)

perform_lookup <- function(match_these, key, value) 
    lookup(key, value)[match_these]

# Returns TRUE if obj is a lookup object
#' @param obj an object
#' @export
is.lookup <- function(obj) inherits(obj, "lookup")

# Function for getting the key from a lookup (can also use lookup$key)
#' @param lookup a lookup object
#' @export
keys <- function(x) UseMethod("keys")

#' @export
keys.lookup <- function(x) x$keys

# Function for getting the value from a lookup (can also use lookup$value)
#' @param lookup a lookup object
#' @export
values <- function(x) UseMethod("values")

#' @export
values.lookup <- function(x) x$values

# Coercing lookups to data.frames
#' @export
as.data.frame.lookup <- function(x) {
    data.frame(keys = keys(x), values = values(x),
               stringsAsFactors = FALSE)
}

# Coercing lookups to lists
#' @export
as.list.lookup <- function(x) list(keys = keys(x), values = values(x))

# Length method for lookups
#' @export
length.lookup <- function(x) length(keys(x))

# Print method for lookups
#' @export
print.lookup <- function(lookup, num_entries = 5) {
    
    # Error handling
    assert_that(num_entries > 0)
    
    # Print the first 5 rows by default
    df <- items(lookup)[1:num_entries, ]
    
    # Deal with num_entries that is lesser or greater than length(lookup)
    lookup_len <- length(lookup)
    if (num_entries < lookup_len) {
        
        # If num_entries is less than the number of rows, add .. to the end of
        # each column for printing
        df <- rbind(df, data.frame(keys = "..", values = ".."))
        num_entries <- num_entries + 1
        
    } else if (num_entries > lookup_len) {
        num_entries <- lookup_len
    }
    
    # Print the length and first X entries of df
    cat(paste0("Lookup: Length[", lookup_len, "]"), "\n\n")
    print(head(df, num_entries), row.names = FALSE)
}

# Check is a lookup object contains a key
#' @export
contains_keys <- function(x, key) UseMethod("contains_keys")

#' @export
contains_keys.lookup <- function(x, key) key %fin% x$keys

# Check is a lookup object contains a value
#' @export
contains_values <- function(x, key) UseMethod("contains_values")

#' @export
contains_values.lookup <- function(x, key) key %fin% x$values

# Coerce objects to lookups
#' @export
as_lookup <- function(x) UseMethod("as_lookup")

# Convert lists to lookups - uses first two elements as the keys and values
# of the lookup, respectively
#' @param x a list
#' @param keys (optional) the element of your list to become the keys
#' @param values (optional) the element of your list to become the values
#' @export
as_lookup.list <- function(x, keys = NULL, values = NULL) {
    
    if (!is.null(keys) & !is.null(values)) {
        the_lookup <- lookup(x[[keys]], x[[values]])
        
    } else if (is.null(keys) & is.null(values)) {
        the_lookup <- lookup(x[[1]], x[[2]])
        if (length(x) > 2) 
            warning("List has more than two elements, so the first two ",
                    "elements are being used as the keys and values, ",
                    "respectively.")
        
    } else {
        stop("You must either specify which elements of your list will ",
             "become the keys and values of the lookup, or leave both ",
             "blank and let the first two elements become the keys and ",
             "the values, respectively.")
    }
    
    the_lookup
}

# Converts data.frames to lookups - uses first two columns as the keys and
# values of the lookup, respectively
#' @export
as_lookup.data.frame <- function(x) lookup(x[[1]], x[[2]])

# Deletes one or more keys from the lookup

# Method for fast subsetting of a lookup object
#' @param lookup the lookup object
#' @param i the values to be matched on (must be a vector)
#' @param nomatch_error (default is FALSE) if any elements in the vector 
#' \code{i} are not keys in the lookup object \code{lookup}, a value of 
#' \code{TRUE} results in an error. For the default of \code{FALSE},  
#' non-matches are represented by \code{NA} values.
#' @export

`[.lookup` <- function(lookup, i, nomatch_error = TRUE) {
    
    # Get keys, values
    keys <- keys(lookup)
    values <- values(lookup)
    
    # Error handling
    assert_that(is_vector(i))
    if (nomatch_error) {
        unique_i <- unique(i)
        differences <- unique_i[fastmatch::fmatch(unique_i, keys, 0L) == 0L]
        if (length(differences) != 0)
            stop("One or more values that you are asking to be matched ",
                 "isn't in the keys of the lookup. Please set nomatch_error ",
                 "to FALSE to turn off this behavior.\n\nValues not found: ",
                 head(differences))
    }
     
    # Do the quick matching
    values[fastmatch::fmatch(i, keys)]
}

# Function to delete one or more keys from the lookup
#' @export
delete_keys <- function(lookup, ...) UseMethod("delete_keys")

#' @export
delete_keys.lookup <- function(lookup, ...) {
    
    # Initialize keys to delete, error handling
    delete_keys <- c(...)
    if (!all(contains_keys(lookup, delete_keys)))
        stop("One or more of the keys you would like to delete do not exist",
             " in the lookup.")
    
    # Delete the keys
    delete_positions <- fastmatch::fmatch(delete_keys, keys(lookup))
    lookup$keys <- keys(lookup)[-delete_positions]
    lookup$values <- values(lookup)[-delete_positions]
    
    lookup
}

# Functions to delete one or more values from the lookup
#' @export
delete_values <- function(lookup, ...) UseMethod("delete_values")

#' @export
delete_values.lookup <- function(lookup, ...) {
    
    # Initialize keys to delete, error handling
    delete_values <- c(...)
    if (!all(contains_values(lookup, delete_values)))
        stop("One or more of the values you would like to delete do not exist",
             " in the lookup.")
    
    # Delete the values
    df <- as.data.frame(lookup)
    df <- df[!df$values %in% delete_values, ]
    lookup <- as_lookup(df)
    
    lookup
}

# Function that, given two types, says which will be coerced to which
coerce_what <- function(c1, c2) {
    
    # Coercion lookup
    cl <- lookup(c("logical", "integer", "double", "factor", "character"), 
                 c(1, 2, 3, 4, 5))
    c1n <- cl[c1]
    c2n <- cl[c2]
    
    # Coersion object
    cobject <- list(coercion = FALSE, coerce_this = NULL, to_that = NULL,
                    coerce_index = NULL, factor_warning = FALSE)
    
    if (c1 == "factor" & c2 != "factor" | c2 == "factor" & c1 != "factor") {
        return("Factor conversion")
        
    } else if (c1 < c2) {
        cobject$coersion <- TRUE
        cobject$coerce_this <- c1n
        cobject$to_that <- c2n
        cobject$coerce_index = 1
        
    } else if (c1 > c2) {
        cobject$coersion <- TRUE
        cobject$coerce_this <- c2n
        cobject$to_that <- c1n
        cobject$coerce_index = 2
        
    } else {
        # No coersion
    }
    
    cobject
}

# Function to merge two lookups
merge_lookups <- function(lookup1, lookup2, ...) {
    
    # Getting around single dispatch
    assert_that(is.lookup(lookup2))
    
    # Combine the keys and values for the two lookups
    all_keys <- c(keys(lookup1), keys(lookup2))
    all_values <- c(values(lookup1), values(lookup2))
    
    # Create a single lookup from the two lookups
    lookup(all_keys, all_values, ...)
}

# Nicer function to merge two lookups
#' @export
`+.lookup` <- function(x, y) merge_lookups(x, y)

# Updates existing values in a lookup
update_lookup <- function(lookup, keys, values) {
    
    # Error handling
    if (any(!keys %in% keys(lookup))) 
        stop("One or more keys do not exist in the lookup")
    
    # Update the lookup
    key_positions <- fastmatch::fmatch(keys, keys(lookup))
    lookup$values[key_positions] <- values
    
    lookup
}

add_lookup <- function(lookup, keys, values) {
    
    # Error handling
   
     if (any(keys %in% keys(lookup))) 
        stop("One or more of your new keys are already in the lookup")
    
    # Add the new keys/values
    added_lookup <- lookup(keys, values)
    lookup$keys <- c(keys(lookup), keys(added_lookup))
    lookup$values <- c(values(lookup), values(added_lookup))
    
    lookup
}

