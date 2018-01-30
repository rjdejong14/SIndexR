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
#' Calcuate age based on a height, site index for a site index curve
#' @description
#' Converts a Height and Site Index to an Age for a particular Site Index
#' Curve.
#' @param curve Integer/Numeric, Defines curve index.
#'                       The particular site index curve to project the height and age along.
#' @param height Numeric, Defines a tree height of the species in meters.
#' @param ageType Integer/Numeric, Defines age type. Must be one of:
#'                        \code{SI_AT_TOTAL}, the age is the total age of the stand in years since
#'                        planting; or \code{SI_AT_BREAST}, the age indicates the number of years since the stand
#'                        reached breast height.
#' @param siteIndex Numeric, Defines site index of the stand.
#' @param y2bh Numeric, The number of years it takes the stand to reach breast height.
#' @return
#'      \code{output} contains computed age; \code{error} contains error information.
#'      If an error condition occurs, the age is set to the
#'        same as the return value.
#'
#'  ------------
#'    0, or an error code under the following conditions:
#'
#'    return value    condition
#'    ------------    ---------
#'    SI_ERR_CURVE    input curve is not a valid curve index.
#'    SI_ERR_GI_MIN   if bhage < 0.5 (for GI)
#'    SI_ERR_GI_MAX   if bhage > GI range
#'    SI_ERR_NO_ANS   if computed height > 999
#'    SI_ERR_GI_TOT   if total age and GI curve
#'    SI_ERR_LT13     if site index <= 1.3
#'
#' @importFrom data.table data.table
#' @rdname SIndexR_HtSIToAge
SIndexR_HtSIToAge <- function(curve,
                              height,
                              ageType,
                              siteIndex,
                              y2bh){
  curve <- wholeToInteger(curve, "curve")
  ageType <- wholeToInteger(ageType, "ageType")
  inputdata <- data.table::data.table(curve, height, ageType, siteIndex, y2bh)
  rm(curve, height, ageType, siteIndex, y2bh)
  curve_list <- as.list(inputdata$curve)
  height_list <- as.list(inputdata$height)
  ageType_list <- as.list(inputdata$ageType)
  siteIndex_list <- as.list(inputdata$siteIndex)
  y2bh_list <- as.list(inputdata$y2bh)
  inputdata_list <- Map(list, curve_list, height_list, ageType_list,
                        siteIndex_list, y2bh_list)

  age <- unlist(lapply(inputdata_list,
                       function(s) index_to_age(cu_index = s[[1]],
                                                site_height = s[[2]],
                                                age_type = s[[3]],
                                                site_index = s[[4]],
                                                y2bh = s[[5]])))
  error <- age
  error[error > 0] <- 0
  return(list(output = age,
              error = error))
}
