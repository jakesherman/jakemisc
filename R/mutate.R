library(data.table)
library(dplyr)
library(datasets)
data(mtcars)

View(mtcars)
mtcars <- mutate(mtcars, jake = mpg+3, josh = mpg + 4)

mutate <- function(.data, ..., ref = FALSE) {
    
    if (ref == TRUE) {
        return(mutate(.data, ...))
    } else if (is.data.table(.data)) {
        
        .data[, ]
        
    } else {
        return(mutate(.data, ...))
    }
    
}

library(jakemisc)
