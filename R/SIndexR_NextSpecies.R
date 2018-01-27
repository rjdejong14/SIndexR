#' @title
#' Next species defined in Sindex
#' @description
#' Given a species index, returns the next species defined in Sindex.
#' @param sp_index Integer/Numeric, Specifies species index.
#' @return
#' Integer species index, for use in other Sindex functions.
#'    May return an error code under the following conditions:
#'
#'    return value    condition
#'    ------------    ---------
#'    SI_ERR_SPEC     input parameter is not a valid species index
#'    SI_ERR_NO_ANS   input parameter is last defined species index
#' @note
#' No assumption should be made about the ordering of the species.
#' @rdname SIndexR_NextSpecies
SIndexR_NextSpecies <- function(sp_index){
  sp_index <- wholeToInteger(sp_index, "sp_index")
  return(unlist(lapply(sp_index, function(s) Sindex_NextSpecies(s))))
}
