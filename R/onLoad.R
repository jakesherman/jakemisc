.onLoad <- function(libname, pkgname) {
    
    # stackoverflow.com/questions/12598242/global-variables-in-packages-in-r
    # Want to have package global variables that allow you to turn warnings
    # on or off for all jakemisc functions, and set ref on or off for all 
    # functions. Based on SOF research, it looks like this approach is the best
    # one! 
    assign("pkg_globals", new.env(), envir=parent.env(environment()))
    
    # Set warnings and ref to TRUE by default
    assign("warnings", TRUE, pkg_globals)
    assign("ref", TRUE, pkg_globals)
}