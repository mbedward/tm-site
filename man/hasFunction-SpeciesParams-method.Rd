% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/SpeciesParams-class.R
\docType{methods}
\name{hasFunction,SpeciesParams-method}
\alias{hasFunction,SpeciesParams-method}
\title{Checks if an optional function has been provided.}
\usage{
\S4method{hasFunction}{SpeciesParams}(object, fnName)
}
\arguments{
\item{object}{\code{SpeciesParam} object}

\item{fnName}{name of the function slot (partial matching is used)}
}
\value{
TRUE if the function slot contains a function object
}
\description{
Checks if a \linkS4class{SpeciesParams} object has an optional function in
the given function slot.
}
\examples{
\dontrun{
params <- new("SpeciesParams", name="test")

## Set one of the optional function objects
params@recruit_canopy_fn <- function(area) area^(1.1)

## Check for function - partial name is permitted
hasFunction(params, "recruit")

## returns TRUE
}
}

