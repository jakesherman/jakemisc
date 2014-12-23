#' packages()
#'
#' Either loads, or installs and then loads, one or more packages from either 
#' CRAN or from public repos on Github/Bitbucket. This function is useful when
#' sharing code with collaborators who may or may not have certain packages
#' installed. It can also save time vs. typing out many library functions or
#' devtools::install_github/devtools::install_bitbucket functions. 
#' 
#' You also have the option of putting a double colon (::) after a package name  
#' to just install a package if it isn't installed, so that you may use :: to 
#' access functions from within the package. 
#' 
#' While the install argument is set to TRUE by default, and will download/
#' install any packages specificed that you don't have, you may also change the
#' install argument so that it prompts the user to install packages
#' 
#' @keywords load, install, package, packages, CRAN, Github, 
#' @param ... one or more package names, each seperated by a comma. You do not
#' have to put a comma around the individual package names, though you may do so.
#' @param install do you want to install packages that a user doesn't have
#' installed? Default is TRUE, your other options are FALSE, or "prompt," where
#' a user in an interactive session will be prompted as to whether or not they
#' want packages() to install the packages for them or not.
#' @param results Should a results table be included in output showing which
#' packages loaded, newly_installed, or failed to do either? TRUE or FALSE, 
#' FALSE by default.
#' @param order what order do you want to load packages into the search list? 
#' Your choices are ascending or descending, with ascending being the default.
#' Ascending means that packages are added to the search list in the order they
#' are inputted to the function - packages further to the right will be closer
#' to the global environment.
#' @export
#' @examples
#' 
#' First, let's try replicating library(pryr) with packages():
#' 
#' packages(pryr)
#' 
#' The packages() function takes multiple package names, and may be used to 
#' load/install multiple packages, for example:
#' 
#' packages(pryr, data.table)
#' 
#' In addition to packages from CRAN, you can install packages from public repos
#' on Github or Bitbucket. To install a Github package, in a string type the
#' GitHub username, followed by a /, followed by the repo name, like so:
#' 
#' packages("jakesherman/jakemisc")
#' 
#' For Bitbucket packages, use the "$" symbol:
#' 
#' packages(jakesherman$jakemisc)
#' 
#' Mixing CRAN and Github packages:
#' 
#' packages(pryr, data.table, "RODBC", "jaker/jakesherman")
#' 
#' You may also add a double colon after a package name to indicate that you
#' want the package to be installed, but you do not want to explicitly attach
#' the package. The idea here is that you are going to load the package when
#' you use :: to call a function from a package. See an example:
#' 
#' packages(plyr, "dplyr::")
#' 
#' Note that this function uses non-standard evaluation for the package names,
#' meaning that you dont need to put quotes around the names of your packages.
#' You may do so however if you choose, doing so won't affect the function. The
#' exception is that any package name with a double colon, ::, needs to be in
#' quotes:
#' 
#' packages("pryr", data.table, "RODBC::", jaker/jakesherman)
#' packages(pryr, "data.table::", "RODBC::", "jaker/jakesherman")

packages <- function(..., install = TRUE, results = FALSE, 
                     order = "ascending") {
       
    ## Error handling ----------------------------------------------------------
    
    # install must be TRUE, "prompt", or FALSE
    if (!any(c(TRUE, "prompt", FALSE) %in% install)) {
        stop(paste0("the install argument can only be set to TRUE, 'prompt',",
                    "or FALSE"))
    }
    
    # If install is set to "prompt" and the user isn't in an interactive 
    # session, set install to FALSE, and return a warning
    if (install == "prompt" & !(interactive())) {
        install <- FALSE
        warning(paste0("install was set to 'prompt' in a non-interactive sessi",
                       "on , therefore install was set to FALSE"))
    }
        
    # results must be TRUE or FALSE
    if (!any(c(TRUE, FALSE) %in% results)) {
        stop(paste0("the results argument can only be set to TRUE or FALSE"))
    }
    
    # order must be ascending or descending
    if (!any(c("ascending", "descending") %in% order)) {
        stop(paste0("the order argument can only be set to ascending or ", 
                    "descending"))
    }
    
    ## Define functions --------------------------------------------------------
    
    # Given a string a symbol, return the position of the symbol w/i the string
    getSymbolPosition <- function(string, symbol) {
        return(gregexpr(symbol, string)[[1]][1])
    }
    
    # Get user/repo name from a GitHub package
    getGitHubInfo <- function(package_name, username_and_repo = FALSE) {
        symbolPosition <- getSymbolPosition(package_name, "/")
        GitHub_username <- substr(package_name, 1, (symbolPosition - 1))
        repo <- substr(package_name, (symbolPosition + 1), 
                       nchar(package_name))
        if (username_and_repo) {
            return(paste0(GitHub_username, "/", repo))
        } else {
            return(list(GitHub_username = GitHub_username, repo = repo))
        }
    }
    
    # Get user/repo name from a Bitbucket package
    getBitbucketInfo <- function(package_name, username_and_repo = FALSE) {
        symbolPosition <- getSymbolPosition(package_name, "\\$")
        Bitbucket_username <- substr(package_name, 1, (symbolPosition - 1))
        repo <- substr(package_name, (symbolPosition + 1), 
                       nchar(package_name))
        if (username_and_repo) {
            return(paste0(Bitbucket_username, "/", repo))
        } else {
            return(list(Bitbucket_username = Bitbucket_username, repo = repo))
        }
    }
    
    # Create a function that, given a package name, installs a package from
    # eithern GitHub, Bitbucket, or CRAN
    installIndividualPackage <- function(package_name) {
        
        # Is the package from GitHub?
        if (grepl("/", package_name)) {
            
            # Get the username_and_repo
            GitHubInfo <- getGitHubInfo(package_name, TRUE)
            
            # Install the package from GitHub
            devtools::install_github(GitHubInfo)
            
        # Is the package from Bitbucket?
        } else if (grepl("\\$", package_name)) {
            
            # Get the repo, Bitbucket username
            BitbucketInfo <- getBitbucketInfo(package_name, TRUE)
            
            # Install the package from Bitbucket
            devtools::install_bitbucket(BitbucketInfo) 
            
        # Otherwise, install the package from CRAN:    
        } else {
            install.packages(package_name)
        }  
        
        return(invisible())
    }
    
    # Create a function that, given a package name and the install argument,
    # loads a package, or installs it from CRAN, GitHub, or Bitbucket. If ::
    # is present in the package name, it will be installed if it isn't already
    # installed (assuming install == TRUE), but will not be loaded into the
    # search list
    loadInstallPackage <- function(package_name, install) {
        
        ## Check if there are :: in a package name
     
        # Initialize the isdbcolon vector
        isdbcolon <- FALSE
        
        # If :: are present, note it, remove :: from the package name
        if (grepl("::", package_name)) {
            isdbcolon <- TRUE
            package_name <- sub("::", "", package_name)
        }
        
        ## Get **just** the name of the package by truncating the repo of a
        ## GitHub/Bitbucket package from the GitHub/Bitbucket username
        
        if (grepl("/", package_name)) {
            
            # Assign the GitHub repo name to just_package_name
            just_package_name <- getGitHubInfo(package_name)[["repo"]]
            
        } else if (grepl("\\$", package_name)) {
            
            # Assign the Bitbucket repo name to just_package_name
            just_package_name <- getBitbucketInfo(package_name)[["repo"]]
            
        }  else {
            
            # For CRAN packages
            just_package_name <- package_name
        }
        
        # Remove names from just_package_name
        just_package_name <- unname(just_package_name)
        
        ## Try loading, then installing and loading, the package
        
        if (isdbcolon == TRUE) {
            
            # If :: are present, see if the package is in the installed list,
            # if it isn't then install it 
            if (!(just_package_name %in% all_packages) & install == TRUE) {
                
                # Try installing the package
                installIndividualPackage(package_name)
                
                # Update all_packages
                all_packages <- installedPackages()
                
                # If the package installed, add it to the installed but not
                # loaded list, if it didn't, add it to the failed list
                if (just_package_name %in% all_packages) {
                    
                    results_table[["installed_but_not_loaded"]] <<- 
                        append(results_table[["installed_but_not_loaded"]], 
                               just_package_name)
                    
                    results_table[["newly_installed"]] <<- append(
                        results_table[["newly_installed"]], just_package_name)
                    
                } else {
                    
                    # Package couldn't install properly
                    print(paste0("Could not install ", just_package_name))
                    results_table[["failure"]] <<- 
                        append(results_table[["failure"]], just_package_name)   
                }
                
            } else if (!(just_package_name %in% all_packages) & 
                           install == FALSE) {
                
                # The package isn't installed, and the user has chosen not to
                # install any packages
                results_table[["failure"]] <<- append(results_table[["failure"]], 
                                                      just_package_name)
                
            } else {
                
                # The package is already installed, add it to the installed
                # but not loaded list
                results_table[["installed_but_not_loaded"]] <<- 
                    append(results_table[["installed_but_not_loaded"]], 
                           just_package_name)
            }
            
        } else if (just_package_name %in% all_packages) {  
            
            if (suppressWarnings(require(just_package_name, 
                                         character.only = TRUE))) {
                
                # Package loaded successfully
                results_table[["loaded"]] <<- append(results_table[["loaded"]], 
                                                     just_package_name)
            } else {
                
                # Package couldn't load or install but it is in the 
                # installed list
                print(paste0("Package", just_package_name, "appears to",
                             "be installed, but could not load."))
                
                results_table[["failure"]] <<- append(results_table[["failure"]], 
                                                      just_package_name)
            }
            
        } else if (install == FALSE) {
            
            # The package isn't installed, and the user has chosen not to
            # install any packages
            results_table[["failure"]] <<- append(results_table[["failure"]], 
                                                  just_package_name)
            
        } else {
            
            # Package not installed, let's install it
            installIndividualPackage(package_name)
            
            # Now that the package is installed, let's try loading it again
            if (suppressWarnings(require(just_package_name, 
                                         character.only = TRUE))) {
                
                # Package loaded successfully 
                results_table[["loaded"]] <<- append(results_table[["loaded"]], 
                                               just_package_name)
                results_table[["newly_installed"]] <<- append(
                    results_table[["newly_installed"]], just_package_name)
                
                ## !Warnings section! - packages with known install issues
                ## Please feel free to add package issues you've encountered:
                
                # Add a warning for the xlsx package 
                if (package_name == "xlsx") {
                    warning(paste0("In order for xlsx to install correctly,",
                                   " you must have the correct version (32 bit",
                                   " or 64 bit) of JAVA installed"))
                }
                
            } else {
                
                # If package couldn't load or install but it is in the 
                # installed list, throw a special warning
                if (just_package_name %in% all_packages) {
                    print(paste0("Package", just_package_name, "appears to",
                                   "be installed, but could not load."))
                }
                
                # Package couldn't install/load properly :(
                results_table[["failure"]] <<- append(results_table[["failure"]], 
                                                      just_package_name)
            }
        }
        
        # Now that we may have installed new packages, we need to updated the
        # vector of installed packages (particullary because dependencies may
        # have been installed, some of which may be requested by package())
        all_packages <<- installedPackages()
        return(invisible())
    }
    
    # Create a function to determine the loop sequence
    getLoopSequence <- function(order) {
        
        if (order == "ascending") {
            loop_sequence <- 1:length(package_names)
        } else {
            loop_sequence <- length(package_names):1
        } 
        
        return(loop_sequence)
    }
    
    ## Loop over all of the packages, load/install -----------------------------
    
    # Create parameters of interest before initializing the loop - get all 
    # package names, create the results table, get the loop sequence, and of
    # course turn ... into a vector of package names
    package_names <- NSEtoVector(...)
    all_packages <- installedPackages()  # now an internal package function
    results_table <- list(loaded = NULL, installed_but_not_loaded = NULL,
                          newly_installed = NULL, failure = NULL)
    loopSequence <- getLoopSequence(order)
    
    # If no packages are given in ... kill the function
    if (length(package_names) == 0) stop(paste0("Please input the name(s) of ",
                                                "one or more packages"))
    
    # If install == "prompt", prompt the user about whether or not to install
    # packages, and based on his/her input modify the install variable
    if (install == "prompt") {
        
        # Get a clean vector of package names
        package_names_stripped <- sapply(package_names, function(f) {
            if (grepl("::", f)) f <- sub("::", "", f)
            if (grepl("/", f)) {
                f <- getGitHubInfo(f)[["repo"]]
            } else if (grepl("\\$", f)) {
                f <- getBitbucketInfo(f)[["repo"]]
            } 
            return(f)
        }, USE.NAMES = FALSE)
        
        # Figure out which packages aren't installed
        packages_to_install <- package_names_stripped[
            !(package_names_stripped %in% all_packages)]
        
        # If there are one or more packages requested but not installed, prompt
        # the user to enter [y/n] to choose whether or not install them
        if (length(packages_to_install) == 0) {
            install <- FALSE
        } else {
            cat(paste0("The following packages are not installed: \n",
                  paste(packages_to_install, collapse = ", "), ". \n Would you",
                  " like to install these packages now? [y/n]"))
            command <- scan(what = character(), n = 1, quiet = TRUE) 
            
            # Modify the install variable based on user input
            if (command == "y") {
                install <- TRUE
                
            } else if (command == "n") {
                install <- FALSE
                
            } else {
                stop("You inputted something other than [y/n] in the prompt")
            }
        }
    }
    
    # Run loadInstallPackage() for each package
    for (pckg in loopSequence) loadInstallPackage(package_names[pckg], install)
    
    # If there are packages in the failure element of the results table, warn
    # the user about those failed packages
    if (length(results_table[["failure"]]) > 0) {
        warning(paste0("The following packages could not be loaded/installed: ",
                       paste(results_table[["failure"]], collapse = ", ")))
    }
    
    # Output the various results if asked
    if (results) return(results_table)
    return(invisible())
}