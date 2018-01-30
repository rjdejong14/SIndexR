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
#' Next species defined in Sindex
#' @description
#' Given a species index, returns the next species defined in Sindex.
#' @param sp_index Integer/Numeric, Specifies species index.
#' @return
#' Integer species index, for use in other Sindex functions.
#'    May return an error code under the following conditions:
#'
#'    return value    condition
#'    ------------    ---------
#'    SI_ERR_SPEC     input parameter is not a valid species index
#'    SI_ERR_NO_ANS   input parameter is last defined species index
#' @note
#' No assumption should be made about the ordering of the species.
#' @rdname SIndexR_NextSpecies
SIndexR_NextSpecies <- function(sp_index){
  sp_index <- wholeToInteger(sp_index, "sp_index")
  return(unlist(lapply(sp_index, function(s) Sindex_NextSpecies(s))))
}
