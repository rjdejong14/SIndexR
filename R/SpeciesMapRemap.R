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
  if(length(sp_index) == 1 & length(cu_index) != 1){
    sp_index <- rep(sp_index, length(cu_index))
  }
  if(length(sp_index) != 1 & length(cu_index) == 1){
    cu_index <- rep(cu_index, length(sp_index))
  }
  if(length(sp_index) != length(cu_index)){
    stop("sp_index and cu_index do not have same length.")
  }
  sp_index_list <- lapply(sp_index, function(s) s)
  cu_index_list <- lapply(cu_index, function(s) s)
  allinputs <- Map(list, sp_index_list, cu_index_list)
  return(unlist(lapply(allinputs, function(s) Sindex_NextCurve(sp_index = s[[1]],
                                                               cu_index = s[[2]]))))

  return(species_remap (sc, fiz))
}

