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
#' Convert curve index to species index
#' @description
#' Returns species index for a given curve index.
#' @param cu_index Integer/Numeric, Specifies cuive index.
#' @return
#'    Integer species index, for use in other Sindex functions.
#'    May return an error code under the following conditions:
#'
#'    return value    condition
#'
#'    ------------    ---------
#'
#'    SI_ERR_CURVE    input curve is not a valid curve index for any species.
#' @rdname SIndexR_CurveToSpecies
SIndexR_CurveToSpecies <- function(cu_index){
  cu_index <- wholeToInteger(cu_index, "cu_index")
  return(unlist(lapply(cu_index, function(s) Sindex_CurveToSpecies(s))))
}
