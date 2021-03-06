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
#'    Returns string containing species code.
#' @description
#'    Returns string containing species code.
#' @param sp_index Integer, Specifies species index.
#' @return
#'    Pointer to string containing species code.
#'    If input parameter is not a valid species index, the return is the
#'    null pointer.
#' @note
#'    Species code string takes the form "Xx" or "Xxx", such as "Sw" or "Fdc".
#' @rdname SIndexR_SpecCode
SIndexR_SpecCode <- function(sp_index){
  sp_index <- wholeToInteger(sp_index, "sp_index")
  return(unlist(lapply(sp_index, function(s) Sindex_SpecCode(s))))
}
