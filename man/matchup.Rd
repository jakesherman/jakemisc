% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/matchup.R
\name{matchup}
\alias{matchup}
\title{matchup()}
\usage{
matchup(require_matches, matching_data, match_this, to_this)
}
\arguments{
\item{require_matches}{a character vector of elements that need to be matched}

\item{matching_data}{a data frame containing columns matching two things,
i.e. the column names you are inputting in the match_this and to_this
arguments.}

\item{match_this}{column name of what you want matched (ex. state
initials)...}

\item{to_this}{...column name of what you want the above matched to (ex.
state names)}
}
\description{
Created as a quick way to match two variables that have a 1:1
relationship. If more than two matches are found for an input, only the first
one is returned (to get around this, use merge() or a similar, more proper
function).
}
\details{
Another function of mine, \code{createMatchup()}, creates closures
based on this function, which can be useful for creating reuseable matching
functions on the fly.

This function works by taking a data frame ("matching data") and the column
names of
}
\examples{
Imagine that we have a vector of state initials ("MA", "CA", "NY"), and we
want to get state names for those initials. Because there is a 1:1
relationship between state initials and names, matchup() will do the trick.
Supply the matchup() function 1) your vector of state initials, 2) a data
frame containing a column of state initials and a matching column of state
names, 3) the column name of state initials, and 4) the column name of state
names. Below is the code to do so:

matchup(vector_of_state_initials, us_states, "state", "state_name")

And the result is a vector: ("Massachusetts", "Califorina", "New York")
}
\keyword{1:1}
\keyword{crosswalk,}
\keyword{lookup,}
\keyword{match,}
\keyword{matchup,}
\keyword{up,}

