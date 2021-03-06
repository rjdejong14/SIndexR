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
#'  Site index conversion between species
#' @description
#'  Site index conversion between species. This function needs to be revised.
#' @param sp_index1 Integer/Numeric, Source species index.
#' @param site Numeric, Source species site index.
#' @param sp_index2 Integer/Numeric, Target species index.
#' @return
#'    Floating point target species site index. (computed)
#'
#'  Return Value
#'  ------------
#'    0, or an error code under the following conditions:
#'
#'    return value    condition
#'    ------------    ---------
#'    SI_ERR_SPEC     source or target species index is not valid.
#'    SI_ERR_NO_ANS   there is no conversion defined.
#' @importFrom data.table data.table
#' @rdname SIndexR_SIToSI
SIndexR_SIToSI <- function(sp_index1, site, sp_index2){
  sp_index1 <- wholeToInteger(sp_index1, "sp_index1")
  sp_index2 <- wholeToInteger(sp_index1, "sp_index2")
  inputdata <- data.table::data.table(sp_index1, site, sp_index2)
  rm(sp_index1, site, sp_index2)
  sp_index1_list <- as.list(inputdata$sp_index1)
  site_list <- as.list(inputdata$site)
  sp_index2_list <- as.list(inputdata$sp_index2)
  inputdata_list <- Map(list, sp_index1_list, site_list, sp_index2_list)
  site2 <- unlist(lapply(inputdata_list, function(s) Sindex_SITOSI(s[[1]],
                                                                   s[[2]],
                                                                   s[[3]])))
  error <- site2
  error[error > 0] <- 0
  return(list(output = site2, error = error))
}
