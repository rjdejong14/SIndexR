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
#' Convert age to other type
#' @description
#'    Age conversion between age types (total vs breast height)
#' @param cu_index Integer/Numeric, Curve index.
#' @param age1 Numeric, Source age.
#' @param age_type1 Integer/Numeric, Type of source age (SI_AT_BREAST or SI_AT_TOTAL).
#' @param y2bh Numeric, Years to breast height.
#' @param age_type2 Integer/Numeric, Type of target age (SI_AT_BREAST or SI_AT_TOTAL).
#'
#' @return
#'  \code{output} contains computed targe age.
#'  \code{error} contains error information, i.e.,
#'      0, or an error code under the following conditions:
#'
#'    return value    condition
#'    ------------    ---------
#'    SI_ERR_CURVE    input curve is not a valid curve index for this species.
#'    SI_ERR_AGE_TYPE unknown age type.
#' @importFrom data.table data.table
#' @rdname SIndexR_AgeToAge
SIndexR_AgeToAge <- function(cu_index,
                            age1,
                            age_type1,
                            y2bh,
                            age_type2){
  cu_index <- wholeToInteger(cu_index, "cu_index")
  age_type1 <- wholeToInteger(age_type1, "age_type1")
  age_type2 <- wholeToInteger(age_type2, "age_type2")
  inputdata <- data.table::data.table(cu_index, age1, age_type1,
                                      y2bh, age_type2)
  rm(cu_index, age1, age_type1,
     y2bh, age_type2)
  cu_index_list <- as.list(inputdata$cu_index)
  age1_list <- as.list(inputdata$age1)
  age_type1_list <- as.list(inputdata$age_type1)
  y2bh_list <- as.list(inputdata$y2bh)
  age_type2_list <- as.list(inputdata$age_type2)
  inputdata_list <- Map(list, cu_index_list, age1_list,
                        age_type1_list, age_type2_list,
                        y2bh_list)

  age2 <- unlist(lapply(inputdata_list,
                        function(s) age_to_age(cu_index = s[[1]],
                                               age1 = s[[2]],
                                               age1_type = s[[3]],
                                               age2_type = s[[4]],
                                               y2bh = s[[5]])))
  error <- age2
  error[error > 0] <- 0
  return(list(output = age2,
              error = error))
}

