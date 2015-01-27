## jakemisc

The personal R package of Jake Sherman. Contained are misc functions and data that I have created/collected that I hope others may find useful.

## Installation

This package is under development, and therefore is not yet on CRAN. It is easy to from GitHub using [devtools](http://cran.r-project.org/web/packages/devtools/index.html).

```
devtools::install_github("jakesherman/jakemisc")
```

## Functions contained

- `changeColClass`: given a data.frame or data.table, changes the class of each column of a given type to another given type. 
- `changeColName`: Changes the name if a given column in a data.frame or data.table.
- `createMatchup`: creates a closure filling the last three arguments of the `matchup` function, allowing one to create quick look-up functions. 
- `factorToNumeric`: really simple, just converts factors into numerics. 
- `matchup`: a quick way to match two variables with a 1:1 relationship, in the same way that VLOOKUP or INDEX/MATCH from Excel allow for matches between variables with a 1:1 relationship.
- `packages`: installs or loads and then installs one or more R packages from CRAN, Github, or Bitbucket. Users have the option of not installing packages that one does not have, or being prompted to install packages via pacakges' arguments.
- `valuesToNA`: given a data.frame or data.table, converts one or more values into NAs. Allows you to specify specific columns for conversion to take place over.
- `valuesToValue`: given a data.frame or data.table, converts one or more values into a different value. Allows you to specify specific columns for conversion to take place over. 

## Acknowledgements

Thank you to Hadley Wickham for your wonderful [Advanced R Book](http://adv-r.had.co.nz/), which is free and extraordinarily helpful, and to Roger, Jeff, and the [Johns Hopkins Data Science team](https://www.coursera.org/specialization/jhudatascience/1) for a fantastic sequence of R and statistics courses. 