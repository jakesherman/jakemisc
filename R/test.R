library(data.table)
library(jakemisc)
library(magrittr)
library(datasets)

data(mtcars)
mtcars <- data.table(mtcars)

mtcars2 <- mtcars %>%
    changeColName("disp", "jake") %>%
    valuesToNA(c(21.0, 6))