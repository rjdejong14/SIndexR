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
#' Calculates site index based on height, age and site index curve.
#' @description Converts a Height and Age to a Site Index for a particular Site Index
#'              Curve.
#' @param curve Integer/Numeric, Specifies site index curve.
#'                       The particular site index curve to project the height and age along.
#' @param age numeric, Tree age.The age of the trees indicated by the curve selection.  The
#'                     interpretation of this age is modified by the 'ageType' parameter.
#' @param ageType Integer/Numeric, Defines age type. Must be one of:
#'                \code{SI_AT_TOTAL}, the age is the total age of the stand in years since
#'                planting; \code{SI_AT_BREAST}, the age indicates the number of years since the stand
#'                reached breast height.
#' @param height numeric, The height of the species in metres.
#' @param estType Integer/Numeric, Defines estimate type. Must be one of:
#'      v             \code{SI_EST_DIRECT}, compute the site index based on direct equations
#'                    if available.  If the equations are not available,
#'                    then automatically fall to the \code{SI_EST_ITERATE}
#'                    method; \code{SI_EST_ITERATE}, compute the site index based on an iterative
#'                    method which converges on the true site index.
#' @return \code{output} contains computed site index.
#'         \code{error} contains error values.
#'
#'      If an error condition occurs, the site index is set to the
#'        same as the returned site index value.
#'
#'  Return Value
#'  ------------
#'    0, or an error code under the following conditions:
#'
#'    return value    condition
#'    ------------    ---------
#'    SI_ERR_CURVE    input curve is not a valid curve index.
#'    SI_ERR_GI_MIN   if bhage < 0.5
#'    SI_ERR_GI_MAX   if bhage > GI range
#'    SI_ERR_NO_ANS   if computed SI > 999
#'    SI_ERR_GI_TOT   if total age and GI curve
#'
#' @importFrom data.table data.table
#' @rdname SIndexR_HtAgeToSI
SIndexR_HtAgeToSI <- function(curve,
                             age,
                             ageType,
                             height,
                             estType){
  curve <- wholeToInteger(curve, "curve")
  ageType <- wholeToInteger(ageType, "ageType")
  estType <- wholeToInteger(estType, "estType")
  inputdata <- data.table::data.table(curve, age, ageType, height, estType)
  rm(curve, age, ageType, height, estType)
  curve_list <- as.list(inputdata$curve)
  age_list <- as.list(inputdata$age)
  ageType_list <- as.list(inputdata$ageType)
  height_list <- as.list(inputdata$height)
  estType_list <- as.list(inputdata$estType)
  inputdata_list <- Map(list, curve_list, age_list, ageType_list,
                        height_list, estType_list)
  site <- unlist(lapply(inputdata_list, function(s) height_to_index(cu_index = s[[1]],
                                                                    age = s[[2]],
                                                                    age_type = s[[3]],
                                                                    height = s[[4]],
                                                                    si_est_type = s[[5]])))

  error <- site
  error[error >= 0] <- 0
  return(list(output = site,
              error = error))
}
