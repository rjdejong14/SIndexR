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
#' First species defined in Sindex
#' @description
#' Returns a species index for the first species defined in Sindex
#' @return
#' Integer species index, for use in other Sindex functions.
#' @note
#' No assumption should be made about the ordering of the species.
#' @export
#' @rdname SIndexR_FirstSpecies
SIndexR_FirstSpecies <- function(){
  return(Sindex_FirstSpecies())
}
