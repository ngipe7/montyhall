% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-pkg.R
\name{open_goat_door}
\alias{open_goat_door}
\title{Simulate host's selection of a door with at goat behind it.}
\usage{
open_goat_door(game, a.pick)
}
\arguments{
\item{game}{is a character vector.}

\item{a.pick}{is a numeric integer.}
}
\value{
The function returns a numeric vector.
}
\description{
\code{open_goat_door()} uses the functions if and sample to have
the host open a door with a goat behind it.
}
\details{
This function performs the second step in the game where the host
reveals a goat door, leaving the contestant with their choice and one
other door. One of the remaining two doors has a car and the other has a goat.
}
\examples{
open_goat_door()

}
