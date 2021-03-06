% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SIndexR_VersionNumber.R
\name{SIndexR_VersionNumber}
\alias{SIndexR_VersionNumber}
\title{Sindex version}
\usage{
SIndexR_VersionNumber()
}
\value{
The number indicating the version of the Sindex routines.
}
\description{
Returns the version number of the Sindex routines.
}
\note{
The format of the number is always in the form of Mmm:
   where  M:    the major release number (1, 2, ...)
         mm:    the minor release number (0, 1, ..., 99)

   An example would be: 631, meaning version 6.31

   If the major release is greater than what your application expects,
   assume that the Sindex routines cannot be used, and that the user
   needs to obtain a newer version of sindex.dll.

   Minor release changes will include the following:
    - addition of a function
    - changed return values (i.e. error messages)
    - iterating for solutions may generate different results
    - bug fixes in implementation of site index equations
    - addition of species
    - addition of curve sources (equations)
    - change of default curve for a species
    - change of mapping species to a different species
}
