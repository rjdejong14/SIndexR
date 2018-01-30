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
#'    Determine species index from species code
#' @description
#'    Determine species index from species code
#' @param sc Character, Defined as species code.
#' @return Species index.
#'    May return an error code under the following conditions:
#'
#'    return value    condition
#'    ------------    ---------
#'    SI_ERR_CODE     if species code is unknown
#'
#' @note
#'    Species code string can be 1, 2, or 3 letters; upper/lower case
#'      is ignored.
#' @rdname SIndexR_SpecMap
SIndexR_SpecMap <- function(sc)
{
  return(unlist(lapply(sc, function(s) species_map (s))))
}


#' @title
#'  Remap species to recommended species, and return species index
#' @description
#'  Remap species to recommended species, and return species index
#' @param sc Character, Species code.
#' @param fiz Character. Forest inventory zone: (A,B,C)=coast,
#'                       (D,E,F,G,H,I,J,K,L)=interior.
#'
#'  @return Species index.
#'    May return an error code under the following conditions:
#'
#'    return value    condition
#'    ------------    ---------
#'    SI_ERR_CODE     if species code is unknown
#'    SI_ERR_FIZ      if FIZ code is unknown
#'
#' @note
#'    Species code string can be 1, 2, or 3 letters; upper/lower case
#'      is ignored.  FIZ is only used where needed, such as for species
#'      code "FD".
#' @rdname   SIndexR_SpecRemap
SIndexR_SpecRemap <- function(sc, fiz)
{
  if(length(sc) == 1 & length(fiz) != 1){
    sc <- rep(sc, length(fiz))
  }
  if(length(sc) != 1 & length(fiz) == 1){
    fiz <- rep(fiz, length(sc))
  }
  if(length(sc) != length(fiz)){
    stop("sc and fiz do not have same length.")
  }
  sc_list <- lapply(sc, function(s) s)
  fiz_list <- lapply(fiz, function(s) s)
  allinputs <- Map(list, sc_list, fiz_list)
  return(unlist(lapply(allinputs, function(s) species_remap(sc = s[[1]],
                                                               fiz = s[[2]]))))

}

