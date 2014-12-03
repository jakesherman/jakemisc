## jakemisc

The personal R package of Jake Sherman. Contained are misc functions and data that I have created/collected that I hope others may find useful.

## Installation

This package is under development, and therefore is not yet on CRAN. It is easy to from GitHub using [devtools](http://cran.r-project.org/web/packages/devtools/index.html).

```
devtools::install_github("jakesherman/jakemisc")
```

## Functions contained

- `changeColClass`: given a data.frame or data.table, changes the class of a specified column.  
- `changeColName`: given a data.frame or data.table, changes the class of each column of a given type to another given type. 
- `creatematchup`: creates a closure filling the last three arguments of the `matchup` function, allowing one to create quick look-up functions. 
- `factorToNumeric`: really simple, just converts factors into numerics. 
- `matchup`: a quick way to match two variables with a 1:1 relationship, in the same way that VLOOKUP or INDEX/MATCH from Excel allow for matches between variables with a 1:1 relationship.
- `packages`: installs or loads and then installs one or more R packages from CRAN, Github, or Bitbucket. Users have the option of not installing packages that one does not have, or being prompted to install packages via pacakges' arguments.
- `valuesToNA`: given a data.frame or data.table, converts one or more values into NAs. Allows you to specify specific columns for conversion to take place over.
- `valuesToNA`: given a data.frame or data.table, converts one or more values into a different value. Allows you to specify specific columns for conversion to take place over.

### A note on the *changeColClass*, *changeColName*, *valuesToNA*, and *valuesToNA* functions:

Each of these functions is compatible with both data.frames and [data.tables](https://github.com/Rdatatable/data.table). In order to make the syntax for these functions consistent between the two, these functions contain [side effects](https://en.wikipedia.org/wiki/Side_effect_%28computer_science%29), which are frowned upon in functional programming. A side effect is when a function modifies something in additional to returning a value. Let's demonstrate by using the changeColName function to change the name of a column named *Jake* in a data.frame or data.table named `my_data` to *Josh*:

```
changeColName(my_data, "Jake", "Josh)
```

*Data.table* incorporates side effects in order to perform modifications-in-place to avoid copying large data sets. Normally when you perform a function in R, R makes a copy of the entire object that you are performing the operation on. For example, changing the name on a single column in a 50GB data.frame requires R to copy that entire data.frame, change the column name, then return the new data.frame. For (extremely) large data sets, that copying uses up considerable time and memory. As a result, data.table has the ability to do modifications in place and avoid that copying. In this case, data.table's `setnames` function avoids copying, whereas using `names(my_data) <- c(new_names)` creates a copy of your data. To check whether or not a copy of an object is being made in R, use the `pryr::address()` function on an object before, and then after a modification is made to it. 

Instead of having different syntax for data.frame and data.table inputs, I have decided to use side effects, such that these functions will use non-standard evaluation for data.frames to get the name of the object my_data, and replace it with the new object. I want to give people using this package a heads up about the behavior of these functions, and also **to suggest that folks not using data.table consider using it**. It [outperforms ddplyr and Python's Pandas in benchmarking](https://github.com/Rdatatable/data.table/wiki/Benchmarks-%3A-Grouping). I've had great luck with it so far when working with large data sets. 

## Acknowledgements

Thank you to Hadley Wickham for your wonderful [Advanced R Book](http://adv-r.had.co.nz/), which is free and extraordinarily helpful, and to Roger, Jeff, and the [Johns Hopkins Data Science team](https://www.coursera.org/specialization/jhudatascience/1) for a fantastic sequence of R and statistics courses. 