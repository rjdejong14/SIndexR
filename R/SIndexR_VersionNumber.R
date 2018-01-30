# Copyright 2018 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#' @title
#' Sindex version
#' @description
#' Returns the version number of the Sindex routines.
#'
#' @return
#' The number indicating the version of the Sindex routines.
#'
#' @note
#' The format of the number is always in the form of Mmm:
#'    where  M:    the major release number (1, 2, ...)
#'          mm:    the minor release number (0, 1, ..., 99)
#'
#'    An example would be: 631, meaning version 6.31
#'
#'    If the major release is greater than what your application expects,
#'    assume that the Sindex routines cannot be used, and that the user
#'    needs to obtain a newer version of sindex.dll.
#'
#'    Minor release changes will include the following:
#'     - addition of a function
#'     - changed return values (i.e. error messages)
#'     - iterating for solutions may generate different results
#'     - bug fixes in implementation of site index equations
#'     - addition of species
#'     - addition of curve sources (equations)
#'     - change of default curve for a species
#'     - change of mapping species to a different species
#' @export
#' @rdname SIndexR_VersionNumber
SIndexR_VersionNumber <- function(){
  return(Sindex_VersionNumber())
}
