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
#' Returns default curve index for a species and establishment type.
#' @description
#' Returns default curve index for a species and establishment type.
#' @param sp_index Integer, Speciefies species index.
#' @param estab Integer, Speciefies establishment type.
#' @return
#'    Integer curve index, for use in other Sindex functions.
#'    May return an error code under the following conditions:
#'
#'    return value    condition
#'    ------------    ---------
#'    SI_ERR_SPEC     input parameter is not a valid species index
#'    SI_ERR_ESTAB    input parameter is not a valid establishment type
#'    SI_ERR_NO_ANS   no curves defined for this species
#' @export
#' @rdname SIndexR_DefCurveEst
SIndexR_DefCurveEst <- function(sp_index, estab){
  sp_index <- wholeToInteger(sp_index, "sp_index")
  estab <- wholeToInteger(estab, "estab")

  if(length(sp_index) == 1 & length(estab) != 1){
    sp_index <- rep(sp_index, length(estab))
  }
  if(length(sp_index) != 1 & length(estab) == 1){
    estab <- rep(estab, length(sp_index))
  }
  if(length(sp_index) != length(estab)){
    stop("sp_index and estab do not have same length.")
  }
  sp_index_list <- lapply(sp_index, function(s) s)
  estab_list <- lapply(estab, function(s) s)
  allinputs <- Map(list, sp_index_list, estab_list)
  return(unlist(lapply(allinputs, function(s) Sindex_DefCurveEst(sp_index = s[[1]],
                                                                  estab = s[[2]]))))


}
