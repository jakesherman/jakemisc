## jakemisc

The personal R package of Jake Sherman. Contained are misc. functions and data that others may find useful.

## Installation

This package is under development, and therefore is not yet on CRAN. It is easy to from GitHub using [devtools](http://cran.r-project.org/web/packages/devtools/index.html).

```
devtools::install_github("jakesherman/jakemisc")
```

## Functions contained in jakemisc

- `changeColClass`: given a data.frame or data.table, changes the class of a specified column. Contains side effects in order to achieve consistent synatax for data.frames and data.tables. Contains side effects in order to achieve consistent synatax for data.frames and data.tables. 
- `changeColName`: given a data.frame or data.table, changes the class of each column of a given type to another given type. 
- `creatematchup`: creates a closure filling the last three arguments of the `matchup` function, allowing one to create quick look-up functions. 
- `matchup`: a quick way to match two variables with a 1:1 relationship, in the same way that VLOOKUP or INDEX/MATCH from Excel allow for matches between variables with a 1:1 relationship.
- `packages()`: installs or loads and then installs one or more R packages from CRAN, Github, or Bitbucket. Users have the option of not installing packages that one does not have, or being prompted to install packages via pacakges' arguments.

## Acknowledgements

To Roger Peng and Hadley Wickham for each sharing his R knowledge with the world free of charge. 