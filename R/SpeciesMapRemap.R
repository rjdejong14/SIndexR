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
#' @rdname Sindex_SpecMap
Sindex_SpecMap <- function(sc)
{
  return(species_map (sc))
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
#' @rdname   Sindex_SpecRemap
Sindex_SpecRemap <- function(sc, fiz)
{
  return(species_remap (sc, fiz))
}

