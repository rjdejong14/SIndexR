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
#'    Calculate a height based on an age, site index and site index curve.
#' @description
#'    Converts an Age and Site Index to a Height for a particular Site Index
#'    Curve.
#' @param curve Integer/Numeric, The particular site index curve to project the height and age along.
#' @param age Numeric, The age of the trees indicated by the curve selection.  The
#'                     interpretation of this age is modified by the 'ageType' parameter.
#' @param ageType Integer/Numeric, Age type. Must be one of:
#'                \code{SI_AT_TOTAL}, the age is the total age of the stand in years since
#'                planting, or \code{SI_AT_BREAST}, the age indicates the number of years since the stand
#'                reached breast height.
#' @param siteIndex Numeric, The site index value of the stand.
#' @param y2bh Numeric, Years to breast height.
#'                      The number of years it takes the stand to reach breast height.
#' @return \code{output} the computed height
#'         \code{error} 0, or an error code under the following conditions:
#'
#'    return value    condition
#'    ------------    ---------
#'    SI_ERR_CURVE    input curve is not a valid curve index.
#'    SI_ERR_GI_MIN   if bhage < 0.5 (for GI)
#'    SI_ERR_GI_MAX   if bhage > GI range
#'    SI_ERR_NO_ANS   if computed age > 999
#'    SI_ERR_GI_TOT   if total age and GI curve
#'    SI_ERR_LT13     if site index <= 1.3
#' @importFrom data.table data.table
#' @rdname SIndexR_AgeSIToHt
#'
SIndexR_AgeSIToHt<- function(curve,
                             age,
                             ageType,
                             siteIndex,
                             y2bh){
  curve <- wholeToInteger(curve, "curve")
  ageType <- wholeToInteger(ageType, "ageType")
  inputdata <- data.table::data.table(curve, age, ageType,
                                      siteIndex, y2bh)
  rm(curve, age, ageType,
     siteIndex, y2bh)
  curve_list <- as.list(inputdata$curve)
  age_list <- as.list(inputdata$age)
  ageType_list <- as.list(inputdata$ageType)
  siteIndex_list <- as.list(inputdata$siteIndex)
  y2bh_list <- as.list(inputdata$y2bh)
  rm(inputdata)
  inputdata_list <- Map(list, curve_list, age_list,
                        ageType_list, siteIndex_list, y2bh_list)
  height <- unlist(lapply(inputdata_list,
                          function(s) index_to_height(cu_index = s[[1]], iage = s[[2]],
                                                      age_type = s[[3]],
                                                      site_index = s[[4]],
                                                      y2bh = s[[5]], pi = 0.5)))
  error <- height
  error[error > 0] <- 0
  return(list(output = height,
              error = error))
}
