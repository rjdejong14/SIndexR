% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SIndexR_SpecCode.R
\name{SIndexR_SpecCode}
\alias{SIndexR_SpecCode}
\title{Returns string containing species code.}
\usage{
SIndexR_SpecCode(sp_index)
}
\arguments{
\item{sp_index}{Integer, Specifies species index.}
}
\value{
Pointer to string containing species code.
   If input parameter is not a valid species index, the return is the
   null pointer.
}
\description{
Returns string containing species code.
}
\note{
Species code string takes the form "Xx" or "Xxx", such as "Sw" or "Fdc".
}
