## Tests - one set of tests if you have data.table

# Test 1 - if data.table is installed ///////////////////////////
if (require(data.table)) {
    
    library(datasets)
    mtcars <- data.table(mtcars)
    mtcars2 <- copy(mtcars)
    mtcars3 <- copy(mtcars)
    mtcars3 <- as.data.frame(mtcars)
    
    # mtcars is a data.table, do modification by reference. mtcars2 is the same
    # data.table by don't do modification by reference. mtcars3 is a data.frame.
    # Then, check to see if they are all identical.
    valuesToNA(mtcars, c(6,4))
    mtcars2 <- valuesToNA(mtcars2, c(6,4), ref = FALSE)
    mtcars3 <- valuesToNA(mtcars3, c(6,4))
    mtcars3 <- data.table(mtcars3)
    
    identical(mtcars, mtcars2)
    identical(mtcars, mtcars3)
    identical(mtcars2, mtcars3)  
}

# Test 2 /////////////////////////////////////////////////////

library(datasets)
mtcars <- valuesToNA(mtcars, c(6,4))

# See if any 6s or 4s exist in mtcars
mtcars_false <- sapply(mtcars, function(f) {
    all(c(6,4) %in% f)
})

if(any(mtcars_false)) {
    print("failure")
} else {
    print("success")
}