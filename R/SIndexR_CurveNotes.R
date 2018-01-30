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
#' Curve notes
#' @description
#'    Returns string containing notes on use.
#' @param cu_index Integer/Numeric, Curve index.
#' @return A string containing notes on use of curve.
#'    If input parameter is not a valid curve index, the return is the
#'    null pointer.
#' @rdname SIndexR_CurveNotes
SIndexR_CurveNotes <- function(cu_index){
  cu_index <- wholeToInteger(cu_index, "cu_index")
  return(unlist(lapply(cu_index, function(s) Sindex_CurveNotes(s))))
}
