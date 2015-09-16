## ============================================================================
##
## fillna() - inspired by pandas.fillna()
##
## ============================================================================

# Method dispatch for fillna
fillna <- function(data, ..., inplace = FALSE) UseMethod("fillna")

# Data.table method for fillna
fillna.data.table <- function(data, ..., fill_only = NULL, no_fill = NULL,
                              inplace = FALSE) {
    
    # If inplace is TRUE, use data.table's set function, otherwise use the
    # data.frame method
    
    if (inplace) {
        
        # Turn ... into a list
        dot_args <- list(...)
        
        # Error handling ------------------------------------------------------
        
        # General errors
        assert_that(length(dot_args) > 0)
        if (!is.null(fill_only) & !is.null(no_fill))
            stop("fill_only and no_fill may not be used simultaneously")
        if ((!is.null(fill_only) | !is.null(no_fill)) & length(dot_args) > 1)
            stop("If fill_only or no_fill is set to TRUE you cannot specify ",
                 "specific columns to fill on")
        
        # Make sure column names are correct
        if (length(dot_args) > 1) assert_that(namesIn(names(dot_args), data))
        if (!is.null(fill_only)) assert_that(namesIn(fill_only, data))
        if (!is.null(no_fill)) assert_that(namesIn(no_fill, data))
        
        # Data.table must be installed for inplace to work - it does not need
        # to be in the search list because set is accessed via ::
        if (!isPackageInstalled("data.table")) {
            warning("Data.table not installed, so inplace cannot be set to ",
                    "TRUE - returning the result while setting it to FALSE")
            return(fillna(data, ..., fill_only = fill_only, no_fill = no_fill,
                          inplace = FALSE))
        }
        
        # Do NA filling -------------------------------------------------------
        
        if (length(dot_args) == 1) {
            
            # When the length of dot_args == 1, we are looping through one
            # or more col_names and modifying those columns in place
            col_names <- names(data)
            if (!is.null(fill_only)) {
                col_names <- fill_only
                
            } else if (!is.null(no_fill)) {
                col_names <- col_names[!col_names %in% no_fill]
            }
            
            # Fill the NAs by reference
            for (col_name in col_names)
                data.table::set(data, which(is.na(data[[col_name]])), col_name, 
                                dot_args[[1]])
        } else {
            
            # When the length of dot_args is > 1, we are filling in specific
            # columns with specific values
            dot_names <- names(dot_args)
            for (i in seq_along(dot_args)) 
                data.table::set(data, which(is.na(data[[dot_names[i]]])),
                                dot_names[i], dot_args[[i]])
        }
        
    } else {
        
        # Use the data.frame method
        data_attributes <- attributes(data)
        class(data) <- "data.frame"
        data <- fillna(data, ..., fill_only = fill_only, no_fill = no_fill)
        attributes(data) <- data_attributes
    }
    
    data
}

# Data.frame method for fillna
fillna.data.frame <- function(data, ..., fill_only = NULL, no_fill = NULL, 
                              inplace = FALSE) {
    
    # Turn ... into a list
    dot_args <- list(...)
    
    # Error handling
    assert_that(length(dot_args) > 0)
    if (inplace) warning("The data.frame method of fillna does not support ",
                         "inplace modification")
    if (!is.null(fill_only) & !is.null(no_fill))
        stop("fill_only and no_fill may not be used simultaneously")
    if ((!is.null(fill_only) | !is.null(no_fill)) & length(dot_args) > 1) {
        stop("If fill_only or no_fill is set to TRUE you cannot specify ",
             "specific columns to fill on")
    }
    
    # Do NA filling -----------------------------------------------------------
    
    if (!is.null(fill_only)) {
        
        # Fill in the fill_only columns with dot_args[[1]]
        data[fill_only] <- lapply(data[fill_only], fillna, dot_args[[1]])
        
    } else if (!is.null(no_fill)) {
        
        # Fill in all columns BUT no_fill with dot_args[[1]]
        valid_columns <- names(data)[!names(data) %in% no_fill]
        data[valid_columns] <- lapply(data[valid_columns], fillna, 
                                      dot_args[[1]])
        
    } else if (length(dot_args) == 1 & is.null(names(dot_args))) {
        
        # Simple NA filling
        data[is.na(data)] <- dot_args[[1]]
        
    } else if (all(names(dot_args) %in% names(data))) {
        
        # Match names NA filling
        dot_names <- names(dot_args)
        for (i in seq_along(dot_args)) 
            data[[dot_names[i]]] <- fillna(data[[dot_names[i]]], dot_args[[i]])
        
    } else {
        stop("Your function input was incorrect")
    }
    
    data
}

fillna.default <- function(data, ..., inplace = FALSE) {
    
    # Turn ... into a list
    dot_args <- list(...)
    assert_that(length(dot_args) == 1)
    
    # Do the conversion
    data[which(is.na(data))] <- dot_args[[1]]
    
    data
}

## Example --------------------------------------------------------------------

test <- data.frame(Jake = c(1, 2, NA, 4), Josh = c(NA, NA, 10, 20))
test

fillna(test, 5)

test %>% fillna(Jake = "hi", Josh = 0)
fillna(test, Jake = 0)
fillna(test, 5, fill_only = "Jake")
fillna(test, 5, no_fill = "Jake")

test <- data.table(test)
setkey(test, Jake)
attributes(test)

test1 <- fillna(test, 5)
attributes(test1)
fillna(test, Jake = "hi", Josh = 0)
fillna(test, Jake = 0)
fillna(test, 5, fill_only = "Jake")
fillna(test, 5, no_fill = "Jake")

# In place modification
fillna(test, Jake = 17, Josh = 0, inplace = TRUE)

# Other possibilities
fillna(test, 5, -Josh)
fillna(test, 5, no_fill = "Josh")
fillna(mtcars, 10.7, mpg, cyl, drat)
fillna(mtcars, 10, fill_only = "mpg")
fillna(mtcars, 12, fill_only = c("cyl", "disp"))
fillna(mtcars, 12, cyl = 12, disp = 12)
fillna(mtcars, 15, mpg, cyl, disp) 
fillna(mtcars, mpg = 15, disp = 22)


createna(mtcars, c("15", "14", "7.33", "testing"))
createna(mtcars, c("15", "14"), -mpg, -cyl)
createna(mtcars, mpg = c("15", "14"), cyl = "NA")

# Logic: take a variable number of arguments
#
# 1. If length(...) is one, we are using ...[1] as the value to fill NAs with,
#    or the values(s) to create NA on for all columns ("the value to go on")
# 2. If length(...) is > 1, there are two possibilities:
#       a. ...[1] is the value to go on, and the rest of ... are columns to 
#          either use or exclude. We can determine that this is the desired
#          option b/c ... will not have any names
#       b. each ... has a 1. name and 2. value(s) "to go on" for each column
#          name. We can tell that this is the desired behavior if ... has
#          one or more names

# Example 1
fillna(mtcars, 5)
fillna(mtcars, value_to_go_on)
createna(mtcars, c(5, 6, 7, "NA", "na", "NaN"))

# Example 2a 
fillna(mtcars, 5, mpg, cyl, disp)
fillna(mtcars, value_to_go_on, -drat, -wt)
createna(mtcars, c(5, 6, 7, "NA", "na", "NaN"), mpg, disp, vs, am)

# Example 2b
fillna(mtcars, mpg = 5, disp = value_to_go_on)
createna(mtcars, cyl = "na", disp = c("NaN", "NA", -999))

fillna_logic <- function(...) {
    
    SE_args <- sapply(substitute(...()), deparse)
    if (length(SE_args) == 1) {
        
        # Example 1
        print("1")
        arg_value <- eval(parse(text = SE_args[1]))
        print(arg_value)
        
    } else {
        
        # Example 2
        if (is.null(names(SE_args))) {
            
            # Example 2a
            print("2a")
            first_arg <- eval(parse(text = SE_args[1]))
            column_names <- SE_args[2:length(SE_args)]
            print(first_arg)
            print(column_names)
            
        } else {
            
            # Example 2b
            print("2b")
            eval_list <- function(SE_args) {
                eval_list <- vector("list", length(SE_args))
                for (i in seq_along(SE_args)) 
                    eval_list[[i]] <- eval(parse(text = SE_args[[i]]))
                names(eval_list) <- names(SE_args)
                eval_list
            }
            new_list <- eval_list(SE_args)
            print(new_list)
        }
    }
}

# Testing example 1
fillna_logic(5)
value_to_go_on = "Testing"
fillna_logic(value_to_go_on)

# Testing example 2a
fillna_logic(10, mpg, cyl)
fillna_logic(c(10, 40, 0), -disp, -vs)
fillna_logic(c(10, value_to_go_on, 25), mpg, jake)

# Testing example 2b
fillna_logic(mpg = 10, disp = 20)
fillna_logic(jake = c(0, 10, 20), roche = "brothers", kyle = value_to_go_on)

fillna_logic_ <- function(...) {
    
    SE_args <- list(...)
    if (length(SE_args) == 1) {
        
        # Example 1
        print("1")
        arg_value <- unlist(SE_args[1])
        print(arg_value)
        
    } else {
        
        # Example 2
        if (is.null(names(SE_args))) {
            
            # Example 2a
            print("2a")
            first_arg <- unlist(SE_args[1])
            column_names <- unlist(SE_args[2:length(SE_args)])
            print(first_arg)
            print(column_names)
            
        } else {
            
            # Example 2b
            print("2b")
            print(SE_args)
        }
    }
}

# Testing example 1
fillna_logic_(5)
value_to_go_on = "Testing"
fillna_logic(value_to_go_on)

# Testing example 2a
fillna_logic_(10, "mpg", "cyl")
fillna_logic_(c(10, 40, 0), "-disp", "-vs")
fillna_logic_(c(10, value_to_go_on, 25), "mpg", "jake")

# Testing example 2b
fillna_logic_(mpg = 10, disp = 20)
fillna_logic_(jake = c(0, 10, 20), roche = "brothers", kyle = value_to_go_on)

# -----------------------------------------------------------------------------

f1 <- function(data, val) {
    data[is.na(data)] <- val
    data
}

f2 <- function(data, val) {
    data[which(is.na(data))] <- val
    data
}

test <- c(NA, LETTERS, NA, NA, NA, NA)
god <- sample(test, 10000000, TRUE)

library(microbenchmark)

microbenchmark(f1(god, 1), f2(god, 1))

# which() appears to speed things up
