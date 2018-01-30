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
#' Returns a code telling what functions are available for a curve index.
#' @description
#' Returns a code telling what functions are available for a curve index.
#' @param cu_index Integer/Numeric, Defines curve index.
#'
#' @return
#'    Integer code.
#'    May return an error code under the following conditions:
#'
#'    return value    condition
#'
#'    ------------    ---------
#'
#'    SI_ERR_CURVE    input curve is not a valid curve index.
#' @note
#'      Code bits are set as follows:
#'      0001: ht = fn (si, age)
#'      0010: si = fn (ht, age)
#'      0100: y2bh = fn (si)
#'      1000: si = fn (ht, age) growth intercept
#' @rdname SIndexR_CurveUse
SIndexR_CurveUse <- function(cu_index){
  cu_index <- wholeToInteger(cu_index, "cu_index")
  return(unlist(lapply(cu_index, function(s) Sindex_CurveUse(s))))
}
